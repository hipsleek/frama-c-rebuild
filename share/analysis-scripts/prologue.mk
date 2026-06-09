##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Makefile template prologue for Frama-C/Eva case studies.
# For details and usage information, see the Frama-C User Manual.

# Note: this variable must be defined before including any files
makefile_dir := $(dir $(lastword $(MAKEFILE_LIST)))

# analysis.mk contains the main rules and targets
include $(makefile_dir)/analysis.mk

# Default target
all: eva
