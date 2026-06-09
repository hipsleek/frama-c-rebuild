# -*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

"""This file provides some functions to open and filter source files
before they are used by other scripts. These filters help improve
the efficiency of regex-based heuristics."""

# These filters require external tools, either in the PATH, or in
# environment variables (the latter has higher priority than the former).
# - scc (a fork including option -k), to remove C comments (variable SCC);
# - astyle, to re-indent lines (variable ASTYLE)
# If a tool is absent, the filter is equivalent to a no-op.

# These functions receive a file object (such as produced by open(),
# subprocess.run, or a previous filter) and return a
# file object containing the output. They abort execution in case
# of errors when running the filters. Note that an absent tool
# does _not_ lead to an error.

import external_tool
from pathlib import Path
import sys


def filter_with_scc(input_data: str) -> str:
    scc_bin = "scc" if sys.platform != "win32" else "scc.exe"
    scc = external_tool.get_command(scc_bin, "SCC")
    if scc:
        return external_tool.run_and_check([scc, "-k", "-b"], input_data)
    else:
        return input_data


def filter_with_astyle(input_data: str) -> str:
    astyle_bin = "astyle" if sys.platform != "win32" else "astyle.exe"
    astyle = external_tool.get_command(astyle_bin, "ASTYLE")
    if astyle:
        return external_tool.run_and_check(
            [astyle, "--keep-one-line-blocks", "--keep-one-line-statements"], input_data
        )
    else:
        return input_data


def open_and_filter(filename: Path, apply_filters: bool) -> str:
    # we ignore encoding errors and use ASCII to avoid issues when
    # opening files with different encodings (UTF-8, ISO-8859, etc)
    with open(filename, "r", encoding="ascii", errors="ignore") as f:
        data = f.read()
    if apply_filters:
        data = filter_with_astyle(filter_with_scc(data))
    return data
