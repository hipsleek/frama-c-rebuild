#!/bin/bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Example of installation of this pre-commit hook (client side):
# - (cd .git/hooks/ && ln -s ../../dev/git-hooks/pre-commit.sh pre-commit)

ROOT=$(git rev-parse --show-toplevel)

echo "Pre-commit Hook..."

STAGED=$(git diff --diff-filter ACMR --name-only --cached | sort)

if [ "$STAGED" = "" ];
then
  echo "Empty commit, nothing to do"
  exit 0
fi

"$ROOT/dev/check-files.sh" -c || exit 1
