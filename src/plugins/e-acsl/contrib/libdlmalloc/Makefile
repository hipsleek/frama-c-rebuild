##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

EACSL_DLMALLOC_LIB = libeacsl-dlmalloc.a
EACSL_DLMALLOC_SRC = dlmalloc.c
EACSL_DLMALLOC_OBJ  = dlmalloc.o
# Don't forget to update "e-acsl-gcc" if the flags are updated
EACSL_DLMALLOC_FLAGS = \
  -DHAVE_MORECORE=0 \
  -DHAVE_MMAP=1  \
  -DNO_MALLINFO=1 \
  -DNO_MALLOC_STATS=1 \
  -DMSPACES=1 \
  -DONLY_MSPACES \
  -DMALLOC_ALIGNMENT=32 \
  -DMSPACE_PREFIX="__e_acsl_" \
  -DUSE_LOCKS=1 \
  -DUSE_SPIN_LOCKS=1

RANLIB ?= ranlib

# Caution: don't use $< here if you want to stay compatible with BSD make
# (See https://stackoverflow.com/questions/12904011/bmake-proper-use-of-automatic-variables)
$(EACSL_DLMALLOC_LIB): $(EACSL_DLMALLOC_SRC)
	@echo 'CC           $(EACSL_DLMALLOC_SRC)'
	@$(CC) $(EACSL_DLMALLOC_SRC) -c -O2 -g3 -o$(EACSL_DLMALLOC_OBJ) $(EACSL_DLMALLOC_FLAGS)
	@echo 'AR           $@'
	@$(AR) crus $@ $(EACSL_DLMALLOC_OBJ)
	@echo 'RANLIB       $@'
	@$(RANLIB) $@
