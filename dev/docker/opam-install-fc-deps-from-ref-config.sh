#!/usr/bin/env bash

# Installs Frama-C's dependencies via opam, from the reference configuration

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

deps=$(grep '^- ' $SCRIPT_DIR/reference-configuration.md | grep -v "^- OCaml" | sed 's/^- //' | sed 's/(.*)//' | xargs)

opam install $deps --confirm-level=unsafe-yes
