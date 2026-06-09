#!/usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# OCAML must be set to the right version of OCAML (format: N_MM or N.MM)

set -euxo pipefail

if [ -z ${OCAML+x} ]; then
  echo "OCAML variable must be set to a version of OCaml"
  exit 2
fi

# Normalize version for Nix
OCAML=${OCAML/./_}

nix-shell "./nix/pkgs.nix" -A ocaml-ng.ocamlPackages_$OCAML.frama-c-checkers-shell --run "$1"
