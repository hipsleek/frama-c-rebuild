#!/usr/bin/env python3
# -*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

"""This script finds files containing likely declarations and definitions
for a given function name, via heuristic syntactic matching."""

from pathlib import Path
import sys
import build_callgraph

arg = ""
if len(sys.argv) < 2:
    print(f"usage: {sys.argv[0]} [file1 file2 ...]")
    print("        prints a heuristic callgraph for the specified files.")
    sys.exit(1)
else:
    files = set([Path(f) for f in sys.argv[1:]])

cg = build_callgraph.compute(files)
build_callgraph.detect_recursion(cg)
