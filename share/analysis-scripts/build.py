#!/usr/bin/env python3
# -*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

"""This script uses blug and a build_commands.json file to produce an
analysis GNUmakefile, as automatically as possible.
NOTE: the tool used to produce such files (blug) is not yet publicly
available."""

from __future__ import annotations
import argparse
import functools
import json
import logging
import os
from pathlib import Path
import re
import shutil
import sys
import subprocess

import function_finder
import source_filter

from typing import Callable, TypeVar

script_dir = os.path.dirname(sys.argv[0])

# Command-line parsing ########################################################

parser = argparse.ArgumentParser(
    description="""Produces a GNUmakefile
for analysis with Frama-C. Tries to use a build_commands.json file if
available."""
)
parser.add_argument(
    "--base",
    metavar="DIR",
    default=".",
    help="base directory to use when pretty-printing relative paths (default: PWD)",
    type=Path,
)
parser.add_argument(
    "--debug",
    metavar="FILE",
    help="enable debug mode and redirect output to the specified file",
)
parser.add_argument("--force", action="store_true", help="overwrite files without prompting")
parser.add_argument(
    "--jbdb",
    metavar="FILE",
    default="build_commands.json",
    help="path to JBDB (default: build_commands.json)",
    type=Path,
)
parser.add_argument(
    "--machdep", metavar="MACHDEP", help="analysis machdep (default: Frama-C's default)"
)
parser.add_argument(
    "--main",
    metavar="FUNCTION",
    default="main",
    help="name of the main function (default: main)",
)
parser.add_argument(
    "--no-source-filter",
    action="store_false",
    dest="source_filter",
    help="disable source filters (less precise, but speeds up large projects)",
)
parser.add_argument(
    "--sources",
    metavar="FILE",
    nargs="+",
    help="list of sources to parse (overrides --jbdb)",
    type=Path,
)
parser.add_argument(
    "--targets",
    metavar="FILE",
    nargs="+",
    help="targets to build. When using --sources, only a single target is allowed.",
    type=Path,
)

args = parser.parse_args()
base = args.base
force = args.force
jbdb_path = args.jbdb
machdep = args.machdep
main = args.main
do_filter_source = args.source_filter
sources = args.sources
targets = args.targets
debug = args.debug

debug_level = logging.DEBUG if debug else logging.INFO
# special values for debug filename
if debug == "stdout":
    logging.basicConfig(stream=sys.stdout, level=debug_level, format="[%(levelname)s] %(message)s")
elif debug == "stderr":
    logging.basicConfig(stream=sys.stderr, level=debug_level, format="[%(levelname)s] %(message)s")
elif debug:
    logging.basicConfig(
        filename=debug,
        level=debug_level,
        filemode="w",
        format="[%(levelname)s] %(message)s",
    )
else:
    logging.basicConfig(level=logging.INFO, format="[%(levelname)s] %(message)s")

dot_framac_dir = Path(".frama-c")

# Check required environment variables and commands in the PATH ###############

framac_bin = Path(
    os.getenv("FRAMAC_BIN")
    or sys.exit("error: FRAMAC_BIN not in environment (set by frama-c-script)")
)

under_test = os.getenv("PTESTS_TESTING")

# Prepare blug-related variables and functions ################################

blug = Path(
    os.getenv("BLUG")
    or shutil.which("blug")
    or sys.exit("error: path to 'blug' binary must be in PATH or variable BLUG")
)
blug_dir = blug.resolve().parent
# to import blug_jbdb
sys.path.insert(0, blug_dir.as_posix())
import blug_jbdb
from blug_jbdb import prettify


# Auxiliary functions #########################################################


def call_and_get_output(command: Path, args: list[str]) -> str:
    try:
        return subprocess.check_output([str(command)] + args, stderr=subprocess.STDOUT).decode()
    except subprocess.CalledProcessError as e:
        sys.exit(f"error running command: {command} {args}\n{e}")


def ask_if_overwrite(path: Path) -> None:
    yn = input(f"warning: {path} already exists. Overwrite? [y/N] ")
    if yn == "" or not (yn[0] == "Y" or yn[0] == "y"):
        sys.exit("Exiting without overwriting.")


def insert_lines_after(lines: list[str], line_pattern: str, new_lines: list[str]) -> None:
    re_line = re.compile(line_pattern)
    for i, line in enumerate(lines):
        if re_line.search(line):
            for j, new_line in enumerate(new_lines):
                lines.insert(i + 1 + j, new_line)
            return
    sys.exit(f"error: no lines found matching pattern: {line_pattern}")


# delete the first occurrence of [line_pattern]
def delete_line(lines: list[str], line_pattern: str) -> None:
    re_line = re.compile(line_pattern)
    for i, line in enumerate(lines):
        if re_line.search(line):
            del lines[i]
            return
    sys.exit(f"error: no lines found matching pattern: {line_pattern}")


def replace_line(lines: list[str], line_pattern: str, value: str, all_occurrences=False) -> None:
    replaced = False
    re_line = re.compile(line_pattern)
    for i, line in enumerate(lines):
        if re_line.search(line):
            lines[i] = value
            replaced = True
            if not all_occurrences:
                return
    if replaced:
        return
    sys.exit(f"error: no lines found matching pattern: {line_pattern}")


# replaces '/' and '.' with '_' so that a valid target name is created
def make_target_name(target: Path) -> str:
    return prettify(target).replace("/", "_").replace(".", "_")


def rel_path(path: Path, base: Path) -> str:
    """Return a relative path to the .frama-c directory, if path is relative to base.
    Otherwise, return an absolute path. Typically, base is the parent of the .frama-c directory."""
    try:
        path.resolve().relative_to(base.resolve())  # Fails if path is not inside base
        return os.path.relpath(path, start=dot_framac_dir)
    except ValueError:
        return str(path.resolve())


def pretty_sources(sources: list[Path], base: Path) -> list[str]:
    return [f"  {rel_path(source, base)} \\" for source in sorted(sources)]


def lines_of_file(path: Path) -> list[str]:
    return path.read_text().splitlines()


fc_stubs_copied = False


def copy_fc_stubs() -> Path:
    global fc_stubs_copied
    dest = dot_framac_dir / "fc_stubs.c"
    if not fc_stubs_copied:
        fc_stubs = lines_of_file(lib_dir / "analysis-scripts" / "fc_stubs.c")
        re_main = re.compile(r"\bmain\b")
        for i, line in enumerate(fc_stubs):
            if line.startswith("//"):
                continue
            fc_stubs[i] = re.sub(re_main, main, line)
        if not force and dest.exists():
            ask_if_overwrite(dest)
        with open(dest, "w") as f:
            f.write("\n".join(fc_stubs))
        logging.info("wrote: %s", dest)
        fc_stubs_copied = True
    return dest


# Returns pairs (line_number, has_args) for each likely definition of
# [funcname] in [filename].
# [has_args] is used to distinguish between main(void) and main(int, char**).
@functools.cache
def find_definitions(funcname: str, filename: Path) -> list[tuple[int, bool]]:
    file_content = source_filter.open_and_filter(
        Path(filename), not under_test and do_filter_source
    )
    file_lines = file_content.splitlines(keepends=True)
    newlines = function_finder.compute_newline_offsets(file_lines)
    defs = function_finder.find_definitions_and_declarations(
        True, False, filename, file_content, file_lines, newlines, funcname
    )
    res: list[tuple[int, bool]] = []
    for d in defs:
        defining_line = file_lines[d[2] - 1]
        after_funcname = defining_line[defining_line.find(funcname) + len(funcname) :]
        # heuristics: if there is a comma after the function name,
        # it is very likely the signature contains arguments;
        # otherwise, the function is either defined in several lines,
        # or we somehow missed it. By default, we assume it has no arguments
        # if we miss it.
        has_args = "," in after_funcname
        res.append((d[2], has_args))
    return res


T = TypeVar("T")


def list_partition(f: Callable[[T], bool], l: list[T]) -> tuple[list[T], list[T]]:
    """Equivalent to OCaml's List.partition: returns 2 lists with the elements of l,
    partitioned according to predicate f."""
    l1 = []
    l2 = []
    for e in l:
        if f(e):
            l1.append(e)
        else:
            l2.append(e)
    return l1, l2


def pp_list(l: list[str]) -> list[str]:
    """Applies prettify to a list of sources/targets and sorts the result."""
    return sorted([prettify(Path(e)) for e in l])


def pp_path_list(l: list[Path]) -> list[str]:
    """Applies prettify to a list of sources/targets and sorts the result."""
    return sorted([prettify(e) for e in l])


# End of auxiliary functions ##################################################

sources_map: dict[Path, list[Path]] = {}
if sources:
    if not targets:
        sys.exit("error: option --targets is mandatory when --sources is specified")
    if len(targets) > 1:
        sys.exit(
            "error: option --targets can only have a single target when --sources is specified"
        )
    sources_map[targets[0]] = sources
elif os.path.isfile(jbdb_path):
    # JBDB exists
    jbdb = blug_jbdb.open_and_absolutize_jbdb(jbdb_path)
    filter_source, filter_target = blug_jbdb.get_filters("c-programs")
    # program_targets are those we prefer, and the only ones used in "automatic" mode;
    # non_program_targets are only used if the user specified them in the command line.
    program_targets = []
    non_program_targets = []
    for f in jbdb:
        programs, other_targets = list_partition(filter_target, f["targets"])
        program_targets += programs
        non_program_targets += other_targets
    logging.debug("program_targets: %s", pp_path_list(program_targets))
    logging.debug("non_program_targets: %s", pp_path_list(non_program_targets))
    all_jbdb_targets = program_targets + non_program_targets
    if not all_jbdb_targets:
        sys.exit(f"no targets found in JBDB ({jbdb_path})")
    if not targets:
        # no targets specified in command line; use all programs from JBDB
        targets = program_targets
    logging.info("Computing sources for each target (%d target(s))...", len(targets))
    unknown_targets_from_cmdline = []
    graph = blug_jbdb.build_graph(jbdb)
    for target in targets:
        if target not in all_jbdb_targets:
            unknown_targets_from_cmdline.append(target)
            # do not return immediately; we want to report all invalid targets at once
        else:
            if unknown_targets_from_cmdline != []:
                continue  # keep looping to accumulate all invalid targets, but avoid extra work
            sources = blug_jbdb.collect_leaves(graph, [target])
            c_sources, non_c_sources = list_partition(filter_source, sources)
            explicit_sources = [
                s for s in c_sources if s in blug_jbdb.cmd_args_using_source(jbdb_path, jbdb, s)
            ]
            sources_map[target] = explicit_sources
    if unknown_targets_from_cmdline:
        targets_pretty = "\n".join(unknown_targets_from_cmdline)
        sys.exit("target(s) not found in JBDB:\n{targets_pretty}")
else:
    if not jbdb_path:
        sys.exit("error: either a JBDB or option --sources are required")
    else:
        sys.exit(f"error: invalid JBDB path: '{jbdb_path}'")

logging.debug(
    "sources_map: %s",
    sorted([prettify(k) + ": " + ", ".join(pp_path_list(v)) for (k, v) in sources_map.items()]),
)
logging.debug("targets: %s", pp_list(targets))

# check that source files exist
unknown_sources = sorted({s for sources in sources_map.values() for s in sources if not s.exists()})
if unknown_sources:
    sys.exit("error: source(s) not found:\n" + "\n".join(pp_path_list(unknown_sources)))

# Check that the main function is defined exactly once per target.
# note: this is only based on heuristics (and fails on a few real case studies),
# so we cannot emit errors, only warnings.
# We also need to check if the main function uses a 'main(void)'-style
# signature, to patch fc_stubs.c.

main_definitions: dict[Path, list[tuple[Path, int, bool]]] = {}
for target, sources in sorted(sources_map.items()):
    main_definitions[target] = []
    for source in sources:
        fundefs = find_definitions(main, source)
        main_definitions[target] += [(source, fundef[0], fundef[1]) for fundef in fundefs]
    if main_definitions[target] == []:
        logging.warning(
            "function '%s' seems to be never defined in the sources of target '%s'",
            main,
            prettify(target),
        )
    elif len(main_definitions[target]) > 1:
        logging.warning(
            "function '%s' seems to be defined multiple times in the sources of target '%s':",
            main,
            prettify(target),
        )
        for filename, line, _ in main_definitions[target]:
            print(f"- definition at {filename}:{line}")

# End of checks; start writing GNUmakefile and stubs from templates ###########

if not dot_framac_dir.is_dir():
    logging.debug("creating %s", dot_framac_dir)
    dot_framac_dir.mkdir(parents=True, exist_ok=False)

fc_config = json.loads(call_and_get_output(framac_bin / "frama-c", ["-print-config-json"]))
lib_dir = Path(fc_config["lib_dir"])

# copy fc_stubs if at least one main function has arguments
any_has_arguments = False
for defs in main_definitions.values():
    if any(d[2] for d in defs):
        any_has_arguments = True
        break

if any_has_arguments:
    fc_stubs = copy_fc_stubs()
    for target in sorted(targets):
        if any(d[2] for d in main_definitions[target]):
            logging.debug(
                "target %s has main with args, adding fc_stubs.c to its sources",
                prettify(target),
            )
            sources_map[target].insert(0, fc_stubs)

gnumakefile = dot_framac_dir / "GNUmakefile"

template = lines_of_file(lib_dir / "analysis-scripts" / "template.mk")

if machdep:
    machdeps = fc_config["machdeps"]
    if machdep not in machdeps:
        logging.warning(
            "unknown machdep (%s) not in Frama-C's default machdeps:\n%s",
            machdep,
            " ".join(machdeps),
        )
    replace_line(template, "^MACHDEP = .*", f"MACHDEP = {machdep}")

if jbdb_path:
    insert_lines_after(
        template,
        "^FCFLAGS",
        [f"  -compilation-db {rel_path(jbdb_path, base)} \\"],
    )

targets_eva = [f"  {make_target_name(target)}.eva \\" for target in sorted(targets)]
replace_line(template, "^TARGETS = main.eva", "TARGETS = \\")
insert_lines_after(template, r"^TARGETS = \\", targets_eva)

delete_line(template, r"^main.parse: \\")
delete_line(template, r"^  main.c \\")
for target, sources in sorted(sources_map.items(), reverse=True):
    pp_target = make_target_name(target)
    new_lines = [f"{pp_target}.parse: \\"] + pretty_sources(sources, base) + [""]
    if any(d[2] for d in main_definitions[target]):
        logging.debug(
            "target %s has main with args, adding -main eva_main to its FCFLAGS",
            prettify(target),
        )
        new_lines += [f"{pp_target}.parse: FCFLAGS += -main eva_main", ""]
    insert_lines_after(template, "^### Each target <t>.eva", new_lines)

gnumakefile.write_text("\n".join(template))

logging.info("wrote: %s", gnumakefile)

# write path.mk, but only if it does not exist.
path_mk = dot_framac_dir / "path.mk"
if not force and path_mk.exists():
    logging.info("%s already exists, will not overwrite it", path_mk)
else:
    path_mk.write_text(
        f"""FRAMAC_BIN={framac_bin}
ifeq ($(wildcard $(FRAMAC_BIN)),)
# Frama-C not installed locally; using the version in the PATH
else
FRAMAC=$(FRAMAC_BIN)/frama-c
FRAMAC_GUI=$(FRAMAC_BIN)/frama-c-gui
endif
"""
    )
    logging.info("wrote: %s", path_mk)
