#!/usr/bin/env python3
# -*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

"""This script serves as wrapper to GNU make (when using the analysis-scripts
GNUmakefile template): it parses the output and suggests useful commands
whenever it can, by calling frama-c-script itself."""

from __future__ import annotations
import argparse
import collections
import os
import re
import subprocess
import sys
from functools import partial

# Check if GNU make is available and has the minimal required version
# (4.0). Otherwise, this script will fail.
# We first test with 'gmake', then 'make', then fail.
make_cmd = "gmake"
get_make_major_version_args = (
    r" --version | grep 'GNU Make\s\+\([0-9]\+\)\..*$' | sed -E 's|GNU Make +([0-9]+)\..*|\1|'"
)

cmd = (
    f"command -v {make_cmd} >{os.devnull} && "
    f'test "$({make_cmd} {get_make_major_version_args})" -ge 4 2>{os.devnull}'
)
if os.system(cmd) != 0:
    make_cmd = "make"
    cmd = (
        f"command -v {make_cmd} >{os.devnull} && "
        f'test "$({make_cmd} {get_make_major_version_args})" -ge 4 2>{os.devnull}'
    )
    if os.system(cmd) != 0:
        sys.exit("error: could not find GNU make >= 4.0 (tried 'gmake' and 'make')")

parser = argparse.ArgumentParser(
    description="""
Builds the specified target, parsing the output to identify and recommend
actions in case of failure."""
)
parser.add_argument(
    "--make-dir",
    metavar="DIR",
    default=".frama-c",
    help="directory containing the makefile (default: .frama-c)",
)

(make_dir_arg, args) = parser.parse_known_args()
make_dir = vars(make_dir_arg)["make_dir"]
args = args[1:]

framac_bin = os.getenv("FRAMAC_BIN")
if not framac_bin:
    sys.exit("error: FRAMAC_BIN not in environment (set by frama-c-script)")
framac_script = f"{framac_bin}/frama-c-script"

output_lines = []
cmd_list = [make_cmd, "-C", make_dir] + args
with subprocess.Popen(cmd_list, stdout=subprocess.PIPE, stderr=subprocess.PIPE) as proc:
    while True:
        assert proc.stdout is not None  # for mypy
        line_bytes = proc.stdout.readline()
        if line_bytes:
            sys.stdout.buffer.write(line_bytes)
            sys.stdout.flush()
            output_lines.append(line_bytes.decode("utf-8"))
        else:
            break

re_missing_spec = re.compile(
    "Neither code nor (specification|[^ ]+ assigns clause) for function ([^,]+),"
)
re_recursive_call_start = re.compile("detected recursive call")
re_recursive_call_stack_start = re.compile(r"^\s+stack:")
re_recursive_call_stack_end = re.compile(r"^\[")


class Tip:
    def __init__(self, message: str, action: collections.abc.Callable):
        self.message = message
        self.action = action


tips: list[Tip] = []

lines = iter(output_lines)
for line in lines:
    match = re_recursive_call_start.search(line)
    if match:

        def _action(callstack):
            print(callstack)
            print(
                "Consider patching, stubbing or adding an ACSL "
                + "specification to the recursive call, "
                + "then re-run the analysis."
            )

        while True:
            msg_lines = []
            match = re_recursive_call_start.search(line)
            try:
                while not match:
                    line = next(lines)
                    match = re_recursive_call_start.search(line)
                match = None
                while not match:
                    line = next(lines)
                    match = re_recursive_call_stack_start.search(line)
                match = None
                while not match:
                    msg_lines.append(line)
                    line = next(lines)
                    match = re_recursive_call_stack_end.search(line)
                # note: this ending line can also match re_missing_spec
                tips.append(Tip("Found recursive call at:", partial(_action, "".join(msg_lines))))
                break
            except StopIteration:
                print("** Error: did not match expected regex before EOF")
                assert False
    match = re_missing_spec.search(line)
    if match:
        spec_kind = match.group(1)
        fname = match.group(2)

        def _action(fname):
            out = subprocess.Popen(
                [framac_script, "find-fun", "-C", make_dir, fname],
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
            )
            output = out.communicate()[0].decode("utf-8")
            re_possible_definers = re.compile("Possible definitions for function")
            find_fun_lines = iter(output.splitlines())
            for find_fun_line in find_fun_lines:
                if re_possible_definers.match(find_fun_line):
                    found_files = [next(find_fun_lines)]
                    while True:
                        try:
                            found_files.append(next(find_fun_lines))
                        except StopIteration:
                            if len(found_files) > 1:
                                print(
                                    "Found several files defining function '"
                                    + fname
                                    + "', cannot recommend automatically."
                                )
                                print(
                                    "Check which one is appropriate and add it "
                                    + "to the list of sources to be parsed:"
                                )
                                print("\n".join(found_files))
                            else:
                                print(
                                    "Add the following file to the list of "
                                    + "sources to be parsed:\n"
                                    + found_files[0]
                                )
                            return
            print("Could not find any files defining " + fname + ".")
            print("Find the sources defining it and add them, " + "or provide a stub.")

        tips.append(
            Tip(
                f"Found function with missing {spec_kind}: {fname}\n"
                + "   Looking for files defining it...",
                partial(_action, fname),
            )
        )

if tips != []:
    print("")
    print("***** make-wrapper recommendations *****")
    print("")
    counter = 1
    print("*** recommendation #" + str(counter) + " ***")
    print("")
    for tip in tips:
        if counter > 1:
            print("")
            print("*** recommendation #" + str(counter) + " ***")
        print(str(counter) + ". " + str(tip.message))
        counter += 1
        tip.action()
