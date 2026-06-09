##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Makefile template epilogue for analyses with Frama-C/Eva.
# For details and usage information, see the Frama-C User Manual.

# Some targets provided for convenience
# Note: they all depend on TARGETS having been properly set by the user
eva: $(TARGETS)
parse: $(TARGETS:%.eva=%.parse)
# Opening one GUI for each target is cumbersome; we open only the first target
gui: $(firstword $(TARGETS)).gui
ivette: $(firstword $(TARGETS)).ivette
	$(warning The ivette target is deprecated, use gui)

# Default target
all: eva
ifeq ($(TARGETS),)
	@echo "error: TARGETS is empty"
endif

display-targets:
	@echo "$(addprefix .frama-c/,$(TARGETS))"
