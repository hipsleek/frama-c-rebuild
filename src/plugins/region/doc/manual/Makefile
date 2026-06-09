##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# --------------------------------------------------------------------------
# ---  Region User & Reference Manual                                    ---
# --------------------------------------------------------------------------

.PHONY: all clean install
PLUGIN=region

all: $(PLUGIN).pdf

include ../MakeDoc

clean::
	@echo "Cleaning WP"
	rm -f $(PLUGIN).pdf $(PLUGIN).cb $(PLUGIN).cb2 $(PLUGIN).log $(PLUGIN).out\
		$(PLUGIN).aux $(PLUGIN).blg $(PLUGIN).bbl *~

TEX_FILES=$(shell find . -name "*.tex")
DOT_FILES=$(shell find . -name "*.dot")
MAP_FILES=$(DOT_FILES:.dot=.pdf)
.SUFFIXES: .dot .pdf
.dot.pdf:
	dot -Tpdf $< -o $@

$(PLUGIN).pdf: $(TEX_FILES) $(FRAMAC_DOC) VERSION VERSION_CODENAME $(MAP_FILES)
	latexmk -pdf $(PLUGIN)

rebuild: clean $(PLUGIN).pdf

TARGET=$(FRAMAC_TRUNK)/doc/manuals/$(PLUGIN)-manual.pdf
install:
	rm -f $(TARGET)
	rm -f $(MAP_FILES)
	cp $(PLUGIN).pdf $(TARGET)
