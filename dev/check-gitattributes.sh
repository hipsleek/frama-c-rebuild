#!/bin/bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# This script helps with cleaning up .gitattributes. It does so by identifying
# entries from the .gitattributes in the current directory, which do not match
# any existing file.
# It has not been tested extensively, so do not delete entries solely on its
# output.

if [ ! -f .gitattributes ]; then
  echo "Error: there are no .gitattributes here!"
  exit 1
fi

cat .gitattributes | while read line; do
  [[ "$line" =~ ^# ]] && continue # skip comments
  [[ -z "$line" ]] && continue # skip empty lines
  LINE=$(echo "$line" | sed -e 's/ .*//')
  case "${LINE:0:1}" in
    /) pattern="${LINE:1}";;
    *) pattern="$LINE **/$LINE";;
  esac
  git ls-files -- $pattern | read _ || echo "entry might be obsolete: $line"
done
