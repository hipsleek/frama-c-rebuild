# Customized makefile template for testing 'frama-c-script make-wrapper'.

include $(shell frama-c -print-lib-path)/analysis-scripts/prologue.mk

CPPFLAGS += -D useless_macro

# Note: the -no-autoload-plugins line is necessary for Cram testing, otherwise
#       the test dependencies would need to explicitly enumerate each
#       internalized Frama-C plug-in.
FCFLAGS     += \
  -no-autoload-plugins -load-module eva,inout,metrics,scope \
  -kernel-warn-key annot:missing-spec=abort \
  -kernel-warn-key typing:implicit-function-declaration=abort \

EVAFLAGS    += \
  -eva-warn-key builtins:missing-spec=abort \

## Analysis targets (suffixed with .eva)
TARGETS = make-for-make-wrapper.eva

make-for-make-wrapper.parse: \
  make-wrapper.c \
  make-wrapper2.c \
  # make-wrapper3.c is deliberately absent of this list

### Epilogue. Do not modify this block. #######################################
include $(shell frama-c -print-lib-path)/analysis-scripts/epilogue.mk
###############################################################################
