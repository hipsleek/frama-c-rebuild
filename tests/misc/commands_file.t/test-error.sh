#!/bin/bash -eu

# This script is needed because Dune does not allow setting a specific shell
# for Cram tests; we need to use Bash to avoid issues with older Dash versions
# which do not support 'set -o pipefail'.
# When Dune supports it, replace this script with the following one-liner:
# (set -o pipefail && dune exec --cache=disabled -- frama-c -commands-file \
# with_error.txt src_empty.c -then -print | \
# sed 's/^\(\s*use `\).*\(frama-c -help.*\)$/\1\2/g')

set -o pipefail

dune exec --cache=disabled -- \
     frama-c -no-autoload-plugins -load-module eva,inout,scope -commands-file with_error.txt src_empty.c -then -print \
    | sed 's/^\(\s*use `\).*\(frama-c -help.*\)$/\1\2/g'
