#!/usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# OCAML must be set to the right version of OCAML (format: N_MM or N.MM)

if [[ $# != 1 ]];
then
  cat <<EOF
usage: OCAML=N_MM $0 <nix-target>
  $0 <nix-target> run the nix-build command for this target
EOF
  exit 2
fi

if [ -z ${OCAML+x} ]; then
  echo "OCAML variable must be set to a version of OCaml"
  exit 2
fi

# Normalize version for Nix
OCAML=${OCAML/./_}

OUTOPT=""
if [ ! -z ${OUT+x} ]; then
  OUTOPT="-o $OUT"
fi

if [ -z ${DIR+x} ]; then
  DIR="."
fi

nix-build $OUTOPT $DIR/nix/pkgs.nix -A ocaml-ng.ocamlPackages_$OCAML.$1
