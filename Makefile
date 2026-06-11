##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# This file is the main makefile of Frama-C.

MAKECONFIG_DIR=share
FRAMAC_DEVELOPER?=

include $(MAKECONFIG_DIR)/Makefile.common

##############################################################################
# TOOLS CONFIG
################################

IN_FRAMAC:=yes

FRAMAC_PTESTS_SRC:=tools/ptests
FRAMAC_HDRCK_SRC:=tools/hdrck
FRAMAC_LINTCK_SRC:=tools/lint

##############################################################################
# Frama-C
################################

.PHONY: all

DUNE_WS?=

ifneq (${DUNE_WS},)
  WORKSPACE_OPT:=--workspace dev/dune-workspace.${DUNE_WS}
else
  WORKSPACE_OPT:=
endif

DISABLED_PLUGINS?=
HIPSLEEK_DIR:=hipsleek

all::
ifeq (${FRAMAC_DEVELOPER},yes)
	dune build ${WORKSPACE_OPT} --no-print-directory --root ${FRAMAC_LINTCK_SRC}
	dune build ${WORKSPACE_OPT} --no-print-directory --root ${FRAMAC_HDRCK_SRC}
endif
ifneq ($(DISABLED_PLUGINS),)
	dune clean
	rm -rf _build .merlin
	./dev/disable-plugins.sh ${DISABLED_PLUGINS}
endif
	dune build ${WORKSPACE_OPT} ${DUNE_BUILD_OPTS} @install

.PHONY: hipsleek
hipsleek:
	dune build ${WORKSPACE_OPT} ${DUNE_BUILD_OPTS} $(HIPSLEEK_DIR)/hip.exe $(HIPSLEEK_DIR)/sleek.exe

clean:: purge-tests # to be done before a "dune" command
ifeq (${FRAMAC_DEVELOPER},yes)
	dune clean --no-print-directory --root ${FRAMAC_LINTCK_SRC}
	dune clean --no-print-directory --root ${FRAMAC_HDRCK_SRC}
endif
	dune clean
	rm -rf _build .merlin

##############################################################################
# IVETTE
################################

.PHONY: ivette ivette-api ivette-dev

ivette: all
	@$(MAKE) -C ivette

ivette-dev: all
	@$(MAKE) -C ivette dev

ivette-api: all
	@$(MAKE) -C ivette api

##############################################################################
# HELP
################################

help::
	@echo "Build configuration variables"
	@echo "  - RELEASE: compile in release mode if set to 'yes'"
	@echo "  - DUNE_DISPLAY: parameter transmitted to dune --display option"
	@echo "  - DISABLED_PLUGINS: disable these plugins before (re)building"
	@echo "    (none for enabling all plugins)"

##############################################################################
# INSTALL/UNINSTALL
################################

install:: all

INSTALL_TARGET=Frama-C
include share/Makefile.installation
include ivette/Makefile.installation

ifeq (${FRAMAC_DEVELOPER},yes)

install::
	@echo "Installing frama-c-hdrck and frama-c-lint"
	dune install ${WORKSPACE_OPT} --root ${FRAMAC_HDRCK_SRC} --prefix ${PREFIX} ${MANDIR_OPT} 2> /dev/null
	dune install ${WORKSPACE_OPT} --root ${FRAMAC_LINTCK_SRC} --prefix ${PREFIX} ${MANDIR_OPT} 2> /dev/null

uninstall::
	@echo "Uninstalling frama-c-hdrck and frama-c-lint"
	dune uninstall ${WORKSPACE_OPT} --root ${FRAMAC_HDRCK_SRC} --prefix ${PREFIX} ${MANDIR_OPT} 2> /dev/null
	dune uninstall ${WORKSPACE_OPT} --root ${FRAMAC_LINTCK_SRC} --prefix ${PREFIX} ${MANDIR_OPT} 2> /dev/null

endif

###############################################################################
# HEADER MANAGEMENT
################################

# Part that can be shared for external plugins
include share/Makefile.headers

###############################################################################
# Testing
################################

# PTESTS is internal
FRAMAC_PTESTS:=$(FRAMAC_PTESTS_SRC)/ptests.exe

# WTESTS is internal
FRAMAC_WTESTS:=$(FRAMAC_PTESTS_SRC)/wtests.exe

# Frama-C also has ptest directories in plugins, so we do not use default
PTEST_ALL_DIRS:=tests $(shell find -L src/plugins -type d -name tests)

# Test aliasing definition allowing ./configure --disable-<plugin>
PTEST_ALIASES:=@tests/ptests @src/plugins/ptests \
  @src/kernel_internals/parsing/tests/ptests

# WP tests need WP cache
PTEST_USE_WP_CACHE:=yes

# Part that can be shared for external plugins
include share/Makefile.testing

###############################################################################
# Linters
################################

# Code prettyfication and lint
include share/Makefile.linting

###############################################################################
# Frama-C Documentation
################################

include share/Makefile.documentation
