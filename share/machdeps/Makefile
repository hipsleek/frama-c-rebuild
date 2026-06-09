##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

MANUAL_MACHDEPS=machdep_gcc_x86_16.yaml machdep_msvc_x86_64.yaml

update-all: machdep_*.yaml $(MANUAL_MACHDEPS:%=.%.validated)

machdep_avr_8.yaml \
machdep_avr_16.yaml \
machdep_gcc_x86_32.yaml \
machdep_gcc_x86_64.yaml \
machdep_ppc_32.yaml : \
%.yaml: machdep-schema.yaml make_machdep/make_machdep.py make_machdep/*.c
	@./make_machdep/make_machdep.py -i --from-file $@ --check

machdep_x86_16.yaml machdep_x86_32.yaml machdep_x86_64.yaml: \
machdep_%.yaml: machdep_gcc_%.yaml Makefile
	@sed -e 's/sizeof_fun: .*/sizeof_fun: -1/' \
             -e 's/sizeof_void: .*/sizeof_void: -1/' \
             -e 's/alignof_fun: .*/alignof_fun: -1/' \
             -e 's/alignof_void: .*/alignof_void: -1/' \
             -e 's/alignof_aligned: .*/alignof_aligned: -1/' \
             -e 's/max_extended_alignment: .*/max_extended_alignment: 0/' \
             -e 's/compiler: .*/compiler: generic/' \
             -e 's/machdep_name: *machdep_gcc_\([[:alnum:]]*\)/machdep_name: machdep_\1/' \
             -e '/ *__GCC.*/d' -e '/ *__GNU/d' -e '/ *__gnu/d' -e '/ *GXX/d' \
             $< | grep -v "gcc_alignof" > $@

.%.validated: % machdep-schema.yaml
	@echo "Checking $*"
	@./make_machdep/make_machdep.py --from-file $* --check-only
	@touch $@

.PHONY: check-schema

check-schema:
	@$(foreach machdep, $(wildcard machdep_*.yaml), \
           ./make_machdep/make_machdep.py --from-file $(machdep) --check-only \
           || echo "$(machdep) does not conform to machdep schema";)
