#!/usr/bin/env bash

set -eu

# Script used by the test "fc_libc.c"

errors=0

test_dir=$(pwd)
share_libc="$1"
cd "$share_libc"

for f in *.h */*.h; do
    guard=$(echo "$f" | awk '{print toupper($0)}')
    guard=${guard//\//_}
    guard=${guard//./_}
    if ! [[ $f =~ "__fc_" ]]; then
        guard="__FC_${guard}"
    fi
    # A few headers have no inclusion guards; ignore them
    if grep -q "#ifndef" $f; then
        if grep -q "#define" $f; then
            if ! grep -q "^#define $guard$" $f; then
                echo "did NOT find guard '$guard' in share/libc/$f !"
                errors=$((errors+1))
            fi
        fi
    fi
done

if [ $errors -gt 0 ]; then
    echo "found $errors error(s) in libc"
    exit 1
fi
