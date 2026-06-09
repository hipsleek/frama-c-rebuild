#! /bin/bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

VERSION=$1

if test -z "$VERSION"; then
  echo "Missing argument. Usage is:"
  echo "\$ ./dev/set-dune-version.sh <N.MM>"
  exit 2
fi

# For macOS: use gsed if available, otherwise test if sed is BSD
if command -v gsed &>/dev/null; then
  SED=gsed
else
  if sed --version 2>/dev/null | grep -q GNU; then
    SED=sed
  else
    echo "GNU sed required"
    exit 1
  fi
fi

find . -name dune-project -exec $SED -i -e "s/(lang dune [1-9]\.[0-9]*)/(lang dune ${VERSION})/gI" '{}' ';'
find . -name dune-workspace.* -exec $SED -i -e "s/(lang dune [1-9]\.[0-9]*)/(lang dune ${VERSION})/gI" '{}' ';'

echo "All dune-project related files have been updated".
echo "Remember to update:"
echo "- opam"
echo "- reference configuration"
echo "- external plug-ins"
