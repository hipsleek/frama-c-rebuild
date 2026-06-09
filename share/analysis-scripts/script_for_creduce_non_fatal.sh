#!/bin/bash -e
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Script to reduce a Frama-C non-crashing test case.

# This script must be completed by the user.
# Names between '@'s will be replaced by creduce.sh.
# See the Frama-C User Manual for more details.

cc_out=$(mktemp creduce_cc_XXXXXX.log)
fc_out=$(mktemp creduce_fc_XXXXXX.log)

# We always check that the reduced file remains valid C code.
set -o pipefail
@CPP@ "@BASE@" 2>&1 | tee "$cc_out"
set +o pipefail

### Examples of conditions to be maintained by C-Reduce; copy and adapt
#
# Ensure that the C file contains <expr>
# grep -q expr "@BASE@"
#
# Ensure that the compiler output contains <expr>
# grep -q <expr> "$cc_out"
#
###

########## INSERT CONDITION(S) RELATED TO THE SOURCE HERE ##########

##########

set +e # allow Frama-C to fail so we can retrieve its exit code
set -o pipefail
"@FRAMAC@" "@BASE@" @FCFLAGS@ 2>&1 | tee "$fc_out"
fc_retcode=${PIPESTATUS[0]}
set +o pipefail
set -e

### Examples of conditions to be maintained by C-Reduce; copy and adapt
#
# Ensure that Frama-C emits <expr>
# grep -q <expr> "$fc_out"
#
# Ensure that the exit code of Frama_C is <rc>
# test $fc_retcode -eq <rc>
#
###

########## INSERT CONDITION(S) RELATED TO FRAMA-C HERE ##########

##########

### Cleanup
rm -f "$cc_out" "$fc_out"
