#!/usr/bin/bash -eu
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Script used to quickly generate a Frama-C Docker image based on the
# current state of the repository

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cd "$SCRIPT_DIR/../"
USE_STASH=yes dev/make-distrib.sh
mv "frama-c-current.tar.gz" "$SCRIPT_DIR/docker/frama-c-current.tar.gz"
cd "$SCRIPT_DIR/docker"
FRAMAC_ARCHIVE="frama-c-current.tar.gz" make custom-fast.debian

if command -v podman 2>&1; then
    DOCKER=podman
else
    DOCKER=docker
fi

"$DOCKER" tag frama-c-custom-fast.debian frama-c-current

echo "Created Docker image: frama-c-current"
