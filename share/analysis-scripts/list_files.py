#!/usr/bin/env python3
# -*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

"""This script parses a compile_commands.json file and lists the C files in it."""

# See: http://clang.llvm.org/docs/JSONCompilationDatabase.html

from __future__ import annotations
import sys
import os
import json
import re
from pathlib import Path

if len(sys.argv) < 2:
    # no argument, assume default name
    arg = Path("compile_commands.json")
else:
    arg = Path(sys.argv[1])

if not arg.exists():
    sys.exit(f"error: file '{arg}' not found")


# check if arg has a known extension
def is_known_c_extension(ext):
    return ext in (".c", ".i", ".ci", ".h")


pwd = os.getcwd()
fcmake_pwd = Path(pwd) / ".frama-c"  # pwd as seen by the Frama-C makefile
with open(arg) as data:
    jcdb = json.load(data)
jcdb_dir = arg.parent
files: set[tuple[str, str]] = set()  # set of pairs of (file, file_for_fcmake)
for entry in jcdb:
    if not "file" in entry:
        # ignore entries without a filename
        continue
    filename = Path(entry["file"])
    directory = Path(entry["directory"]) if "directory" in entry else None
    if filename.is_absolute():
        filepath = filename
    elif directory and directory.is_absolute():
        filepath = directory / filename
    elif directory:
        filepath = jcdb_dir / directory / filename
    else:
        filepath = jcdb_dir / filename
    if not is_known_c_extension(filepath.suffix):
        print(f"warning: ignoring file of unknown type: {filepath}")
    else:
        files.add((os.path.relpath(filepath, pwd), os.path.relpath(filepath, fcmake_pwd)))

files_for_fcmake = map(lambda x: (x[1]), files)
print("# Paths as seen by a makefile inside subdirectory '.frama-c':")
print("SRCS=\\\n" + " \\\n".join(sorted(files_for_fcmake)) + " \\")
print("")

files_defining_main = set()
re_main = re.compile(r"(int|void)\s+main\s*\([^)]*\)\s*\{")
for fname, file_for_fcmake in sorted(files):
    try:
        with open(fname, "r") as content_file:
            content = content_file.read()
            res = re.search(re_main, content)
            if res is not None:
                files_defining_main.add(file_for_fcmake)
    except OSError:
        print(
            f"warning: could not read file '{fname}', mentioned in '{arg}'. Skipping check of 'main' function."
        )

if files_defining_main != []:
    print("")
    print(
        "# Possible definition of main function in the following file(s), as seen from '.frama-c':"
    )
    print("\n".join(sorted(files_defining_main)))
