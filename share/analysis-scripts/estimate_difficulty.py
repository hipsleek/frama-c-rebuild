#!/usr/bin/env python3
# -*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

"""This script uses several heuristics to try and estimate the difficulty
of analyzing a new code base with Frama-C."""

import argparse
import glob
import json
import logging
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
from typing import Iterable

import build_callgraph
import external_tool
import fclog
import source_filter

# TODO : avoid relativizing paths when introducing too many ".." ;
# TODO : try to check the presence of compiler builtins
# TODO : try to check for pragmas
# TODO : detect absence of 'main' function (library)

parser = argparse.ArgumentParser(
    description="""
Estimates the difficulty of analyzing a given code base"""
)
parser.add_argument(
    "paths",
    nargs="+",
    help="source files and directories. \
If a directory <dir> is specified, it is recursively explored, as if '<dir>/**/*.[ci]' \
had been specified.",
    type=Path,
)
parser.add_argument(
    "--debug",
    metavar="FILE",
    help="enable debug mode and redirect output to the specified file",
)
parser.add_argument(
    "--verbose",
    action="store_true",
    help="enable verbose output; if --debug is set, output is redirected to the same file.",
)
parser.add_argument(
    "--no-cloc",
    action="store_true",
    help="disable usage of external tool 'cloc', even if available.",
)
args = parser.parse_args()
paths = args.paths
debug = args.debug
no_cloc = args.no_cloc
verbose = args.verbose

under_test = os.getenv("PTESTS_TESTING")

fclog.init(debug, verbose)

### Auxiliary functions #######################################################


def get_dir(path):
    """Similar to dirname, but returns the path itself if it refers to a directory"""
    if path.is_dir():
        return path
    else:
        return path.parent


def collect_files_and_local_dirs(paths) -> tuple[set[Path], set[Path]]:
    """Returns the list of files and directories (and their subdirectories) containing
    the specified paths. Note that this also includes subdirectories which do not
    themselves contain any .c files, but which may contain .h files."""
    dirs: set[Path] = set()
    files: set[Path] = set()
    for p in paths:
        if p.is_dir():
            files = files.union([Path(p) for p in glob.glob(f"{p}/**/*.[chi]", recursive=True)])
            dirs.add(p)
        else:
            files.add(p)
            dirs.add(p.parent)
    local_dirs = {Path(s[0]) for d in dirs for s in os.walk(d)}
    if not files:
        sys.exit(
            "error: no source files (.c/.i) found in provided paths: "
            + " ".join([str(p) for p in paths])
        )
    return files, local_dirs


def extract_keys(l):
    return [list(key.keys())[0] for key in l]


def get_framac_libc_function_statuses(
    framac: Path | None, framac_share: Path
) -> tuple[list[str], list[str]]:
    if framac:
        (_handler, metrics_tmpfile) = tempfile.mkstemp(prefix="fc_script_est_diff", suffix=".json")
        logging.debug("metrics_tmpfile: %s", metrics_tmpfile)
        fc_runtime = framac_share / "libc" / "__fc_runtime.c"
        fc_libc_headers = framac_share / "libc" / "__fc_libc.h"
        subprocess.run(
            [
                framac,
                "-no-autoload-plugins",
                fc_runtime,
                fc_libc_headers,
                "-load-module",
                "metrics",
                "-metrics",
                "-metrics-libc",
                "-metrics-output",
                metrics_tmpfile,
            ],
            stdout=subprocess.DEVNULL,
            check=True,
        )
        with open(metrics_tmpfile) as f:
            metrics_json = json.load(f)
        os.remove(metrics_tmpfile)
    else:
        with open(framac_share / "libc_metrics.json") as f:
            metrics_json = json.load(f)
    defined = extract_keys(metrics_json["defined-functions"])
    spec_only = extract_keys(metrics_json["specified-only-functions"])
    return (defined, spec_only)


def grep_includes_in_file(filename: Path):
    re_include = re.compile(r'\s*#\s*include\s*("|<)([^">]+)("|>)')
    file_content = source_filter.open_and_filter(filename, not under_test)
    i = 0
    for line in file_content.splitlines():
        i += 1
        m = re_include.match(line)
        if m:
            kind = m.group(1)
            header = m.group(2)
            yield (i, kind, header)


def get_includes(files: set[Path]):
    quote_includes: dict[Path, list[tuple[Path, int]]] = {}
    chevron_includes: dict[Path, list[tuple[Path, int]]] = {}
    for filename in files:
        for line, kind, header in grep_includes_in_file(filename):
            if kind == "<":
                includes = chevron_includes[header] if header in chevron_includes else []
            else:
                includes = quote_includes[header] if header in quote_includes else []
            includes.append((filename, line))
            if kind == "<":
                chevron_includes[header] = includes
            else:
                quote_includes[header] = includes
    return chevron_includes, quote_includes


def is_local_header(local_dirs, header):
    for d in local_dirs:
        path = Path(d)
        if Path(path / header).exists():
            return True
    return False


def grep_keywords(keywords: Iterable[str], filename: Path) -> dict[str, int]:
    with open(filename, "r") as f:
        found: dict[str, int] = {}
        for line in f:
            if any(x in line for x in keywords):
                # found one or more keywords; count them
                for kw in keywords:
                    if kw in line:
                        if kw in found:
                            found[kw] += 1
                        else:
                            found[kw] = 1
        return found


def pretty_unsupported_keywords(
    file: Path, unsupported_keywords: dict[str, str], found: dict[str, int]
) -> str:
    res = f"unsupported keyword(s) in {file}: "
    descriptions: list[str] = []
    for kw, count in sorted(found.items()):
        if descriptions:  # not first occurrence
            res += ", "
        res += f" {kw} ({count} line{'s' if count > 1 else ''})"
        descriptions.append(f"{kw} is a {unsupported_keywords[kw]}")
    res += "\n - " + "\n - ".join(descriptions)
    return res


### End of auxiliary functions ################################################

debug = os.getenv("DEBUG")
verbose = False

files, local_dirs = collect_files_and_local_dirs(paths)

score = {
    "recursion": 0,
    "libc": 0,
    "includes": 0,
    "malloc": 0,
    "keywords": 0,
    "asm": 0,
}

framac_bin = os.getenv("FRAMAC_BIN")
if not framac_bin:
    logging.info(
        "Running script in no-Frama-C mode (set FRAMAC_BIN to the directory"
        + " containing frama-c if you want to use it)."
    )
    framac = None
    script_dir = os.path.dirname(os.path.realpath(__file__))
    framac_share = Path(script_dir) / "share"
else:
    framac = Path(framac_bin) / "frama-c"
    framac_share = Path(
        subprocess.check_output([framac, "-no-autoload-plugins", "-print-share-path"]).decode()
    )


if not no_cloc:
    cloc = external_tool.get_command("cloc", "CLOC")
    if cloc:
        data = external_tool.run_and_check(
            [cloc, "--hide-rate", "--progress-rate=0", "--csv"] + list(str(f) for f in files), ""
        )
        datas = data.splitlines()
        [nfiles, _sum, nblank, ncomment, ncode] = datas[-1].split(",")
        nlines = int(nblank) + int(ncomment) + int(ncode)
        logging.info(
            "Processing %d file(s), approx. %d lines of code (out of %d lines)",
            int(nfiles),
            int(ncode),
            nlines,
        )

logging.info("Building callgraph...")
cg = build_callgraph.compute(files)

logging.info("Computing data about libc/POSIX functions...")
libc_defined_functions, libc_specified_functions = get_framac_libc_function_statuses(
    framac, framac_share
)

recursive_cycles: list[tuple[tuple[str, int], list[tuple[str, str]]]] = []
reported_recursive_pairs = set()
build_callgraph.compute_recursive_cycles(cg, recursive_cycles)
for cycle_start_loc, cycle in recursive_cycles:
    # Note: in larger code bases, many cycles are reported for the same final
    # function (e.g. for the calls 'g -> g', we may have 'f -> g -> g',
    # 'h -> g -> g', etc; to minimize this, we print just the first one.
    # This does not prevent 3-cycle repetitions, such as 'f -> g -> f',
    # but these are less common.
    if cycle[-1] in reported_recursive_pairs:
        continue
    reported_recursive_pairs.add(cycle[-1])
    (filename, line) = cycle_start_loc

    def pretty_cycle(cycle):
        (x, y) = cycle[0]
        res = f"{x} -> {y}"
        for x, y in cycle[1:]:
            res += f" -> {y}"
        return res

    logging.info(
        "[recursion] found recursive cycle near %s:%d: %s", filename, line, pretty_cycle(cycle)
    )
    score["recursion"] += 1

callees_list = [callee for (_, callee) in list(cg.edges.keys())]
callees = set(callees_list)
used_headers = set()
logging.info("Estimating difficulty for %d function calls...", len(callees))
warnings = 0

problematic_posix_functions = [
    "_longjmp",
    "_setjmp",
    "longjmp",
    "setjmp",
    "siglongjmp",
    "sigsetjmp",
]

handled_by_variadic = [
    "dprintf",
    "execl",
    "execle",
    "execlp",
    "fcntl",
    "fprintf",
    "fscanf",
    "fwprintf",
    "fwscanf",
    "ioctl",
    "open",
    "openat",
    "printf",
    "scanf",
    "snprintf",
    "sprintf",
    "sscanf",
    "swprintf",
    "swscanf",
    "syslog",
    "wprintf",
    "wscanf",
]

for callee in sorted(callees):

    def callee_status(status, reason):
        global warnings
        if status == "warning":
            warnings += 1
        if status == "warning":
            logging.warning("%s %s", callee, reason)
        else:
            logging.log(fclog.VERBOSE, "%s: %s %s", status, callee, reason)

    if callee in problematic_posix_functions:
        callee_status(
            "warning",
            "is known to be problematic for code analysis",
        )
    elif callee in libc_specified_functions:
        callee_status("good", "is specified in Frama-C's libc")
    elif callee in libc_defined_functions:
        callee_status("ok", "is defined in Frama-C's libc")
    else:
        if callee in handled_by_variadic:
            callee_status("ok", "is handled by the Variadic module")

logging.info("Function-related warnings: %d", warnings)
score["libc"] = warnings

logging.log(
    fclog.VERBOSE,
    "Used POSIX headers:\n%s",
    "\n".join([f"  <{header}>" for header in sorted(used_headers)]),
)

(chevron_includes, quote_includes) = get_includes(files)

logging.info(
    "Estimating difficulty for %d '#include <header>' directives...", len(chevron_includes)
)
non_posix_headers = []
header_warnings = 0

posix_headers = [
    "aio.h",
    "arpa/inet.h",
    "assert.h",
    "complex.h",
    "cpio.h",
    "ctype.h",
    "dirent.h",
    "dlfcn.h",
    "errno.h",
    "fcntl.h",
    "fenv.h",
    "float.h",
    "fmtmsg.h",
    "fnmatch.h",
    "ftw.h",
    "glob.h",
    "grp.h",
    "iconv.h",
    "inttypes.h",
    "iso646.h",
    "langinfo.h",
    "libgen.h",
    "limits.h",
    "locale.h",
    "math.h",
    "monetary.h",
    "mqueue.h",
    "ndbm.h",
    "net/if.h",
    "netdb.h",
    "netinet/in.h",
    "netinet/tcp.h",
    "nl_types.h",
    "poll.h",
    "pthread.h",
    "pwd.h",
    "regex.h",
    "sched.h",
    "search.h",
    "semaphore.h",
    "setjmp.h",
    "signal.h",
    "spawn.h",
    "stdarg.h",
    "stdbool.h",
    "stddef.h",
    "stdint.h",
    "stdio.h",
    "stdlib.h",
    "string.h",
    "strings.h",
    "stropts.h",
    "sys/ipc.h",
    "sys/mman.h",
    "sys/msg.h",
    "sys/resource.h",
    "sys/select.h",
    "sys/sem.h",
    "sys/shm.h",
    "sys/socket.h",
    "sys/stat.h",
    "sys/statvfs.h",
    "sys/time.h",
    "sys/times.h",
    "sys/types.h",
    "sys/uio.h",
    "sys/un.h",
    "sys/utsname.h",
    "sys/wait.h",
    "syslog.h",
    "tar.h",
    "termios.h",
    "tgmath.h",
    "time.h",
    "trace.h",
    "ulimit.h",
    "unistd.h",
    "utime.h",
    "utmpx.h",
    "wchar.h",
    "wctype.h",
    "wordexp.h",
]

unsupported_posix_headers = [
    "complex.h",
    "tgmath.h",
]

for header in sorted(chevron_includes, key=str.casefold):
    if not header.lower().endswith(".h"):
        continue  # ignore included non-header files
    if header in unsupported_posix_headers:
        header_warnings += 1
        logging.warning("included header <%s> is explicitly unsupported by Frama-C", header)
    else:
        logging.log(
            fclog.VERBOSE,
            "included header %s",
            header,
        )
    if is_local_header(local_dirs, header):
        logging.log(fclog.VERBOSE, "ok: included header <%s> seems to be available locally", header)
    elif header not in posix_headers:
        non_posix_headers.append(header)
        header_warnings += 1
        logging.warning("included non-POSIX header <%s>", header)


logging.info("Header-related warnings: %d", header_warnings)
score["includes"] = header_warnings

# dynamic allocation

dynalloc_functions = set(["malloc", "calloc", "free", "realloc", "alloca", "mmap"])
dyncallees = dynalloc_functions.intersection(callees)
if dyncallees:
    logging.info("Calls to dynamic allocation functions: %s", ", ".join(sorted(dyncallees)))
    score["malloc"] = len(dyncallees)

# unsupported C11 or non-standard specific features

unsupported_keywords = {
    "_Complex": "C11 construct",
    "_Imaginary": "C11 construct",
}

for ff in files:
    found = grep_keywords(unsupported_keywords.keys(), ff)
    if found:
        logging.warning(pretty_unsupported_keywords(ff, unsupported_keywords, found))
    score["keywords"] += len(found)

# assembly code

if "asm" in callees or "__asm" in callees or "__asm__" in callees:
    logging.warning("code seems to contain inline assembly ('asm(...)')")
    score["asm"] = 1

logging.info(
    "Overall difficulty score:\n%s",
    "\n".join([k + ": " + str(v) for (k, v) in sorted(score.items())]),
)
