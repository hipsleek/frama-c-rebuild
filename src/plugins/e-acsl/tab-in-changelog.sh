#!/bin/bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Base dir of this script
BASEDIR="$(realpath `dirname $0`)"

# Check that the E-ACSL changelog does not contain <TAB>
# Note: do not use -P, which is not macOS-compatible
tab_lines_count=$(grep -c -e "$(printf '\t')" $BASEDIR/doc/Changelog)
if [ "$tab_lines_count" -ne "0" ]; then
    echo "Found <TAB> in E-ACSL changelog"
    exit 1
fi

exit 0
