#!/usr/bin/env python3
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

"""
Produces a machdep.yaml file for a given architecture.

Prerequisites:

- A C11-compatible (cross-)compiler (with support for _Generic),
- A (cross-)compiler supporting _Static_assert
- A (cross-)compiler supporting _Alignof or alignof

This script tries to compile several source files to extract the
information we need in terms of sizeof, alignof and representation
of the various types defined by the standard (e.g. size_t, wchar_t, ...)

In case some values are not identified, the YAML format can be edited
by hand afterwards.
"""

import argparse
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import logging
import yaml

my_path = Path(sys.argv[0]).parent

parser = argparse.ArgumentParser(prog="make_machdep")
parser.add_argument("-v", "--verbose", action="store_true")
parser.add_argument("-o", type=argparse.FileType("w"), dest="dest_file")
parser.add_argument("--compiler", default="cc", help="which compiler to use; default is 'cc'")
parser.add_argument(
    "--compiler-version",
    default="--version",
    help="option to pass to the compiler to obtain its version; default is --version",
)

parser.add_argument(
    "--machdep-schema",
    default="machdep-schema.yaml",
    help="location of the schema file describing a machdep; default is 'machdep-schema.yaml'",
)

parser.add_argument(
    "--from-file",
    help="reads compiler and arch flags from existing yaml file. Use -i to update it in place",
)
parser.add_argument(
    "-i",
    "--in-place",
    action="store_true",
    help="when reading compiler config from yaml, update the file in place. unused otherwise",
)

parser.add_argument(
    "--cpp-arch-flags",
    nargs="+",
    action="extend",
    help="architecture-specific flags needed for preprocessing, e.g. '-m32'",
)
parser.add_argument(
    "--compiler-flags",
    nargs="+",
    action="extend",
    type=str,
    help="flags to be given to the compiler (other than those set by --cpp-arch-flags); default is '-c'",
)
parser.add_argument(
    "--check",
    action="store_true",
    help="checks that the generated machdep is conforming to the schema",
)
parser.add_argument(
    "--check-only",
    action="store_true",
    help="must be used in conjunction with --from-file to check that the provided input file is conforming to the schema",
)

args, other_args = parser.parse_known_args()

if args.verbose:
    logging.basicConfig(format="%(levelname)s: %(message)s", level=logging.INFO)
else:
    logging.basicConfig(format="%(levelname)s: %(message)s")

if not args.compiler_flags:
    args.compiler_flags = ["-c"]

if not args.cpp_arch_flags:
    args.cpp_arch_flags = []


def make_schema():
    with open(args.machdep_schema, "r") as schema:
        return yaml.safe_load(schema)


schema = make_schema()


def check_machdep(machdep):
    try:
        from jsonschema import validate, ValidationError

        strict_schema = {
            # extract the keys
            "required": [x for x in schema],
            "properties": schema,
            "additionalProperties": False,
        }
        validate(machdep, strict_schema)
        return True
    except ImportError:
        logging.warning("jsonschema is not available: no validation will be performed")
        return True
    except ValidationError as exn:
        logging.warning(f"machdep object is not conforming to machdep schema:\n{exn.message}")
        return False


if args.from_file:
    orig_file = open(args.from_file, "r")
    orig_machdep = yaml.safe_load(orig_file)
    orig_file.close()
    if args.check_only:
        if check_machdep(orig_machdep):
            sys.exit(0)
        else:
            sys.exit(1)
    if not "compiler" in orig_machdep or not "cpp_arch_flags" in orig_machdep:
        raise Exception("Missing fields in yaml file")
    args.compiler = orig_machdep["compiler"]
    if isinstance(orig_machdep["cpp_arch_flags"], list):
        args.cpp_arch_flags = orig_machdep["cpp_arch_flags"]
    else:  # old version of the schema used a single string
        args.cpp_arch_flags = orig_machdep["cpp_arch_flags"].split()


def print_machdep(machdep):
    if args.from_file and args.in_place:
        args.dest_file = open(args.from_file, "w")
    elif args.dest_file is None:
        args.dest_file = sys.stdout
    yaml.dump(machdep, args.dest_file, indent=4, sort_keys=True)


def default_value(typ):
    if typ == "integer":
        return -1
    if typ == "boolean":
        return False
    if typ == "string":
        return ""
    if typ == "list":
        return {}
    logging.warning(f"Unexpected type {typ} in YAML schema")
    return None


def make_machdep():
    machdep = {}
    for key in schema:
        machdep[key] = None
    return machdep


machdep = make_machdep()

compilation_command = [args.compiler]

for flag in args.cpp_arch_flags + args.compiler_flags:
    compilation_command = compilation_command + flag.split(" ")

standard_source_files = [
    # sanity_check is juste here to ensure that the given compiler
    # and flags are coherent. It must be kept at the top of the list.
    ("sanity_check.c", "none"),
    ("sizeof_short.c", "number"),
    ("sizeof_int.c", "number"),
    ("sizeof_long.c", "number"),
    ("sizeof_longlong.c", "number"),
    ("sizeof_ptr.c", "number"),
    ("sizeof_float.c", "number"),
    ("sizeof_double.c", "number"),
    ("sizeof_longdouble.c", "number"),
    ("sizeof_void.c", "number"),
    ("sizeof_fun.c", "number"),
    ("alignof_short.c", "number"),
    ("alignof_int.c", "number"),
    ("alignof_long.c", "number"),
    ("alignof_longlong.c", "number"),
    ("alignof_ptr.c", "number"),
    ("alignof_float.c", "number"),
    ("alignof_double.c", "number"),
    ("alignof_longdouble.c", "number"),
    ("alignof_void.c", "number"),
    ("alignof_fun.c", "number"),
    ("alignof_aligned.c", "number"),
    ("alignof_max_align_t.c", "number"),
    ("size_t.c", "type"),
    ("ssize_t.c", "type"),
    ("wchar_t.c", "type"),
    ("ptrdiff_t.c", "type"),
    ("max_align_t.c", "type"),
    ("intptr_t.c", "type"),
    ("uintptr_t.c", "type"),
    ("int_fast8_t.c", "type"),
    ("int_fast16_t.c", "type"),
    ("int_fast32_t.c", "type"),
    ("int_fast64_t.c", "type"),
    ("uint_fast8_t.c", "type"),
    ("uint_fast16_t.c", "type"),
    ("uint_fast32_t.c", "type"),
    ("uint_fast64_t.c", "type"),
    ("wint_t.c", "type"),
    ("sig_atomic_t.c", "type"),
    ("time_t.c", "type"),
    ("char_is_unsigned.c", "bool"),
    ("little_endian.c", "bool"),
    ("has__builtin_va_list.c", "has__builtin_va_list"),
    ("weof.c", "macro"),
    ("wordsize.c", "macro"),
    ("posix_c_source.c", "macro"),
    ("limits_macros.c", "macro"),
    ("stdio_macros.c", "macro"),
    ("stdlib_macros.c", "macro"),
    ("nsig.c", "macro"),
    ("errno.c", "macrolist"),
]

gcc_alignof_source_files = [
    ("gcc_alignof_short.c", "number"),
    ("gcc_alignof_int.c", "number"),
    ("gcc_alignof_long.c", "number"),
    ("gcc_alignof_longlong.c", "number"),
    ("gcc_alignof_ptr.c", "number"),
    ("gcc_alignof_float.c", "number"),
    ("gcc_alignof_double.c", "number"),
    ("gcc_alignof_longdouble.c", "number"),
    ("gcc_alignof_void.c", "number"),
    ("gcc_alignof_fun.c", "number"),
    ("gcc_alignof_aligned.c", "number"),
    ("gcc_alignof_max_align_t.c", "number"),
]

if args.compiler == "gcc":
    source_files = standard_source_files + gcc_alignof_source_files
else:
    source_files = standard_source_files


def find_value(name, typ, output):
    if typ == "bool":
        expected = "(True|False)"

        def conversion(x):
            return x == "True"

        default = False

    elif typ == "number":
        expected = "([0-9]+)"

        def conversion(x):
            return int(x)

        default = -1

    elif typ == "type":
        expected = "`([^`]+)`"

        def conversion(x):
            return x

        default = ""

    else:
        logging.warning(f"unexpected type '{typ}' for field '{name}', skipping")
        return

    if name in machdep:
        msg = re.compile(name + " is " + expected)
        res = re.search(msg, output)
        if res:
            value = conversion(res.group(1))
            if args.verbose:
                print(f"[INFO] setting {name} to {value}")
            machdep[name] = value
        else:
            logging.warning(
                f"cannot find value of field '{name}', using default value: '{default}'\ncompiler output is:\n{output}"
            )
            machdep[name] = default
    else:
        logging.warning(f"unexpected symbol '{name}', ignoring")


def cleanup_cpp(output):
    lines = output.splitlines()
    macro = filter(lambda s: s != "" and s[0] != "#", lines)
    macro = map(lambda s: s.strip(), macro)
    return " ".join(macro)


def find_macros_value(output, is_list=False, entry=None):
    msg = re.compile(r"(\w+)_is = ([^;]+);")
    if is_list:
        assert entry
        machdep[entry] = {}
    for res in re.finditer(msg, output):
        name = res.group(1)
        value = res.group(2).strip()
        if is_list:
            machdep[entry][name] = value
        else:
            if name in machdep:
                if args.verbose:
                    print(f"[INFO] setting {name} to {value}")
                machdep[name] = value
            else:
                logging.warning(f"unexpected symbol '{name}', ignoring")
    if args.verbose:
        print(f"compiler output is:{output}")


with tempfile.TemporaryDirectory() as tmpdirname:
    if args.verbose:
        print(f"[INFO] created temporary directory {tmpdirname}")

    for f, typ in source_files:
        p = my_path / f
        cmd = compilation_command + [str(p)]
        if typ in ("macro", "macrolist"):
            # We're just interested in expanding a macro,
            # treatment is a bit different than the rest.
            cmd = cmd + ["-E"]
        if args.verbose:
            print(f"[INFO] running command: {' '.join(cmd)}")
        proc = subprocess.run(cmd, capture_output=True, cwd=tmpdirname)
        if typ == "none":
            if proc.returncode != 0:
                logging.critical("cannot compile sample C file with provided compiler and flags.")
                logging.info(f"compiler output is:{proc.stderr.decode()}")
                sys.exit(1)
            continue
        if typ == "macro":
            if proc.returncode != 0:
                logging.warning(
                    f"error in preprocessing value '{p}', some values won't be filled\ncompiler output is:\n{proc.stderr.decode()}"
                )
                name = p.stem
                if name in machdep:
                    machdep[name] = ""
                continue
            find_macros_value(cleanup_cpp(proc.stdout.decode()))
            continue
        if typ == "macrolist":
            name = p.stem
            if proc.returncode != 0:
                logging.warning(
                    f"error in preprocessing value '{p}', some values might not be filled\ncompiler output is:{proc.stderr.decode()}"
                )
                if name in machdep:
                    machdep[name] = {}
                continue
            find_macros_value(cleanup_cpp(proc.stdout.decode()), is_list=True, entry=name)
            continue
        if typ == "has__builtin_va_list":
            # Special case: compilation success determines presence or absence
            machdep["has__builtin_va_list"] = proc.returncode == 0
            continue
        if proc.returncode == 0:
            # all tests should fail on an appropriate _Static_assert
            # if compilation succeeds, we have a problem
            logging.warning(f"could not identify value of '{p.stem}', skipping")
            continue
        find_value(p.stem, typ, proc.stderr.decode())

    version_output = subprocess.run(
        [args.compiler, args.compiler_version], capture_output=True, text=True, cwd=tmpdirname
    )
    version_lines = version_output.stdout.splitlines()
    if not version_lines:
        logging.warning(
            "could not obtain compiler version with "
            + f"'{args.compiler_version}'"
            + ", check option --compiler-version"
        )
        version = "<unknown>"
    else:
        version = version_output.stdout.splitlines()[0]

    # Special case for determining whether extended alignments are supported, and,
    # if yes what is the maximal value allowed. We assume that it does not depend
    # on the exact compilation context.
    def test_possible_alignas(align_test):
        f = my_path / "max_extended_alignment.c"
        cmd = compilation_command + [f"-DALIGN_TEST={align_test}", str(f)]
        if args.verbose:
            print(f"[INFO] running command: {' '.join(cmd)}")
        proc = subprocess.run(cmd, capture_output=True, cwd=tmpdirname)
        return proc.returncode == 0

    align_test = 1
    if machdep["alignof_max_align_t"] is not None:
        align_test = 2 * machdep["alignof_max_align_t"]

    machdep["max_extended_alignment"] = -1

    while test_possible_alignas(align_test):
        machdep["max_extended_alignment"] = align_test
        align_test = 2 * align_test

    machdep["compiler"] = args.compiler
    machdep["cpp_arch_flags"] = args.cpp_arch_flags
    machdep["version"] = version

    machdep["custom_defs"] = dict()

    # Extract predefined macros; we're assuming a gcc-like compiler here.
    # Leave custom_defs empty if this fails.

    cmd = compilation_command + ["-dM", "-E", "-"]
    if args.verbose:
        print(f"[INFO] running command: {' '.join(cmd)}")
    proc = subprocess.run(
        cmd, stdin=subprocess.DEVNULL, capture_output=True, text=True, cwd=tmpdirname
    )
    if proc.returncode == 0:
        custom = dict()
        for line in proc.stdout.splitlines():
            # Preprocessor emits a warning if we're trying to #undef
            # standard macros. Leave them alone.
            if re.match(r"#define *__STDC", line):
                continue
            macro = re.match(r"# *define *([^ ]*) *(.*)", line)
            if not macro:
                # This skips over ifndef/endif blocs for msvc, maybe this
                # will be a problem later.
                continue
            macro_var = macro.group(1)
            macro_val = macro.group(2)
            # Python >= 3.7: dict is guaranteed to preserve insertion order
            custom[macro_var] = macro_val
        machdep["custom_defs"] = custom
    else:
        logging.warning(
            f"could not determine predefined macros. compiler output is:\n{proc.stderr}"
        )

    if args.from_file and args.in_place:
        machdep["machdep_name"] = Path(args.from_file).stem
    elif args.dest_file:
        machdep["machdep_name"] = Path(args.dest_file.name).stem
    else:
        machdep["machdep_name"] = "anonymous_machdep"

    def is_optional(field):
        return "optional" in schema[field] and schema[field]["optional"]

    machdep = {f: v for [f, v] in machdep.items() if not (is_optional(f) and v is None)}

    missing_fields = [f for [f, v] in machdep.items() if v is None]

    if missing_fields:
        msg = ", ".join(missing_fields)
        logging.warning(f"the following fields are missing from the machdep definition: {msg}")
        for field in missing_fields:
            machdep[field] = default_value(schema[field]["type"])

    print_machdep(machdep)
