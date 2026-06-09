#!/usr/bin/env bash

set -e

if test -z "$FRAMAC"; then echo "variable FRAMAC must be set"; exit 1; fi
TMPDIR=$(mktemp -d fc_test_libc_XXXXXXXX)
$FRAMAC -print-machdep-header > $TMPDIR/__fc_machdep.h
$FRAMAC -print-machdep-builtin-macros > $TMPDIR/__fc_builtin_macros.h
gcc -I$TMPDIR -D__FC_MACHDEP_X86_64 $@ -Wno-attributes -std=c99 -Wall -Wwrite-strings -Wno-builtin-macro-redefined -Wno-unknown-warning-option -o /dev/null
rm -fr $TMPDIR
