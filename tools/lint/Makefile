##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

FRAMAC_SHARE?=../../share

all:
	dune build --root .

.PHONY: clean
clean:
	dune clean --root .

INSTALL_TARGET=frama-c-lint
include ${FRAMAC_SHARE}/Makefile.common
include ${FRAMAC_SHARE}/Makefile.installation

##########################################################################
