#!/usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Check that if the target branch is stable/... in a MR then the DEFAULT branch
# of the pipeline is also stable/...

echo "=== DEBUG ==="
echo "CI_PIPELINE_SOURCE: $CI_PIPELINE_SOURCE"
echo "DEFAULT: $DEFAULT"
echo "CI_MERGE_REQUEST_TARGET_BRANCH_NAME: $CI_MERGE_REQUEST_TARGET_BRANCH_NAME"
echo "CI_DEFAULT_BRANCH: $CI_DEFAULT_BRANCH"
echo "CI_COMMIT_REF_NAME: $CI_COMMIT_REF_NAME"
echo "============="
echo ""

if grep -q -e '^stable/[a-z]*$' <<< "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME"; then
  echo "Target branch name is stable/..."
  if [ "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME" = "$DEFAULT" ]; then
    echo "Default branch is equal to target branch, allow pipeline to run"
  else
    echo ""
    echo "DIFFERENCE BETWEEN TARGET BRANCH AND DEFAULT BRANCH"
    echo " - target: $CI_MERGE_REQUEST_TARGET_BRANCH_NAME"
    echo " - default: $DEFAULT"
    echo ""
    echo "MAKE SURE THAT THE TARGET BRANCH OF THE MR IS CORRECTLY SET AND THAT"
    echo "THE MR IS CORRECTLY REBASED ON THE LATEST VERSION OF THE STABLE"
    echo "BRANCH."
    exit 1
  fi
else
  echo "Target branch name is *not* stable/..., allow pipeline to run"
fi
