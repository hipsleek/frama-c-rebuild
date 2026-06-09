#!/usr/bin/env python3
# -*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

"""This script uses heuristics to list all function definitions and
declarations in a set of files."""

from pathlib import Path
import sys
import os
import function_finder

debug = os.getenv("DEBUG")

arg = ""
if len(sys.argv) < 4:
    print("usage: %s want_defs want_decls file..." % sys.argv[0])
    print("       looks for likely function definitions and/or declarations")
    print("       in the specified files.")
    sys.exit(1)


def boolish_string(s: str) -> bool:
    if s.lower() == "true" or s == "1":
        return True
    if s.lower() == "false" or s == "0":
        return False
    sys.exit(f"error: expected 'true', 'false', 0 or 1; got: {s}")


want_defs = boolish_string(sys.argv[1])
want_decls = boolish_string(sys.argv[2])
files = set([Path(f) for f in sys.argv[3:]])

for f in sorted(files):
    with open(f, encoding="ascii", errors="ignore") as data:
        file_content = data.read()
    file_lines = file_content.splitlines(keepends=True)
    newlines = function_finder.compute_newline_offsets(file_lines)
    defs_and_decls = function_finder.find_definitions_and_declarations(
        want_defs, want_decls, f, file_content, file_lines, newlines
    )
    for funcname, is_def, start, end, _offset in defs_and_decls:
        if is_def:
            print(f"{os.path.relpath(f)}:{start}:{end}: {funcname} (definition)")
        else:
            print(f"{os.path.relpath(f)}:{start}:{end}: {funcname} (declaration)")
