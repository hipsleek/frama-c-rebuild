#!/usr/bin/env bash

# Calls 'opam init' with the OCaml version given as argument

if [ $# -lt 1 ]; then
    echo "error: missing required version number"
    exit 1
fi

if [[ ! $1 =~ ^[0-9] ]]; then
    echo "error: version must start with a number: $1"
    exit 1
fi

echo "Initializing opam repository with version: $1"

opam init --bare --disable-sandboxing --shell-setup

# detect if using musl
if ldd /bin/ls | grep -q musl; then
    opam switch create $1 \
         --packages=ocaml-variants.$1+options,ocaml-option-musl
else
    opam switch create $1 $1
fi
