##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Makefile template for Frama-C/Eva case studies.
# For details and usage information, see the Frama-C User Manual.

### Prologue. Do not modify this block. #######################################
-include path.mk # path.mk contains variables specific to each user
                 # (e.g. FRAMAC, FRAMAC_GUI) and should not be versioned. It is
                 # an optional include, unnecessary if frama-c is in the PATH.
FRAMAC ?= frama-c # FRAMAC is defined in path.mk when it is included, but the
                  # user can override it in the command-line.
ifeq ($(FRAMAC_LIB),)
  FRAMAC_LIB := $(shell $(FRAMAC) -print-lib-path)
endif
include $(FRAMAC_LIB)/analysis-scripts/prologue.mk
###############################################################################

# Edit below as needed. Suggested flags are optional.

MACHDEP = x86_64

## Preprocessing flags (for -cpp-extra-args)
CPPFLAGS    += \

## Other preprocessing and parsing flags (e.g. -cpp-extra-args-per-file)
PARSEFLAGS    += \

## General flags
FCFLAGS     += \
  -add-symbolic-path=..:. \
  -kernel-warn-key annot:missing-spec=abort \
  -kernel-warn-key typing:implicit-function-declaration=abort \

## Eva-specific flags
EVAFLAGS    += \
  -eva-warn-key builtins:missing-spec=abort \
  -eva-warn-key libc:unsupported-spec=abort \
  -eva-warn-key assigns:missing=abort \

## WP-specific flags
WPFLAGS    += \

## GUI-only flags
FCGUIFLAGS += \

## Analysis targets (suffixed with .eva)
TARGETS = main.eva

### Each target <t>.eva needs a rule <t>.parse with source files as prerequisites
main.parse: \
  main.c \

### Epilogue. Do not modify this block. #######################################
include $(FRAMAC_LIB)/analysis-scripts/epilogue.mk
###############################################################################
