#!/bin/sh -eu
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

PWD=$(dirname $0)

exec ssh -o "UserKnownHostsFile ${PWD}/known_hosts" -i "${PWD}/id_ed25519" "$@"
