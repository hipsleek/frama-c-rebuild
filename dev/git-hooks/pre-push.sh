#!/bin/bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Example of installation of this pre-push hook (client side):
# - (cd .git/hooks/ && ln -s ../../dev/git-hooks/pre-push.sh pre-push)

ROOT=$(git rev-parse --show-toplevel)

echo "Pre-push Hook..."

empty=$(git hash-object --stdin </dev/null | tr '[0-9a-f]' '0')

remote=$1

while read local_ref local_oid remote_ref remote_oid
do
    if test "$local_oid" = "$empty"
    then
        # Handle delete
        :
    else
        if test "$remote_oid" = "$empty"
        then
            # New branch, examine commits starting
            # the forking point from master
            remote_oid=$(git merge-base $local_ref master);
        fi
        range="$remote_oid $local_oid";
        "$ROOT/dev/check-files.sh" -p "$range" || exit 1;
    fi;
done

exit 0
