##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

PLUGIN=acsl-importer
DOCUMENT=main

.PHONY: all
all: doc

include ../MakeDoc

.PHONY: doc
doc: $(PLUGIN).pdf

$(PLUGIN).pdf: $(DOCUMENT).pdf
	@echo "Final copy: $@"
	cp $< $@

$(DOCUMENT).pdf: $(DOCUMENT).tex macros.tex user-changes.tex bib.bib $(FRAMAC_DOC)
	@echo "Build $@"
	latexmk -pdf $(DOCUMENT)

.PHONY: clean
clean::
	@echo "Cleaning"
	rm -f $(TMP) $(addprefix $(DOCUMENT)., rubbercache pdf ilg ind log out toc idx lof bbl blg cb cb2 fdb_latexmk fls) *.aux *.hva *~
