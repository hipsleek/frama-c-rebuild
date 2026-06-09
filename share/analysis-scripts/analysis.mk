##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Makefile for Frama-C/Eva case studies.
# This file is included by epilogue.mk, when using template.mk.
# See the Frama-C User Manual for more details.
#
# This Makefile uses the following variables.
#
# FRAMAC        frama-c binary
# FRAMAC_GUI    frama-c gui binary
# CPPFLAGS      preprocessing flags
# PARSEFLAGS    other preprocessing and parsing flags
#               (e.g. -cpp-extra-args-per-file)
# MACHDEP       machdep
# FCFLAGS       general flags to use with frama-c
# FCGUIFLAGS    flags to use with frama-c-gui
# EVAFLAGS      flags to use with the Eva plugin
# EVABUILTINS   Eva builtins to be set (via -eva-builtin)
# EVAUSESPECS   Eva functions to be overridden by specs (-eva-use-spec)
# WPFLAGS       flags to use with the WP plugin
#
# FLAMEGRAPH    If set (to any value), running an analysis will produce an
#               SVG + HTML flamegraph at the end.
#
# AST_DIFF      If set (to any value), enables usage of -ast-diff during parse.
#
# There are several ways to define or change these variables.
#
# With an environment variable:
#   export FRAMAC=~/bin/frama-c
#   make
#
# With command line arguments:
#   make FRAMAC=~/bin/frama-c
#
# In your Makefile, when you want to change a parameter for all analyses:
#   FCFLAGS += -verbose 2
#
# In your Makefile, for a single target :
#   target.eva: FCFLAGS += -main my_main
#
# For each analysis target, you must give the list of sources to be analyzed
# by adding them as prerequisites of target.parse, as in:
#
# target.parse: file1.c file2.c file3.c...
#
# NOTE ABOUT AST_DIFF:
# - If AST_DIFF is set to a non-empty value (e.g. `fcmake AST_DIFF=y`), then
#   during parsing (rule %.parse), we check if there already exists a framac.sav
#   file. If so, we rename it framac.reparse and, instead of parsing from zero,
#   we reload this file and apply -ast-diff before reparsing the sources.

# Test if Makefile is > 4.0
ifneq (4.0,$(firstword $(sort $(MAKE_VERSION) 4.0)))
  $(error This Makefile requires Make >= 4.0 - available at http://ftp.gnu.org/gnu/make/)
endif

# Test if sed has the '--unbuffered' option (GNU sed has, but neither macOS'
# nor Busybox' have it, in which case we ignore it)
SED_UNBUFFERED:=sed$(shell sed --unbuffered //p /dev/null 2>/dev/null && echo " --unbuffered" || true)

# If there is a GNU time in the PATH, which contains the desired options
# (-f and -o), use them; otherwise, ignore it.
# 'env' allows bypassing shell builtins (if they exist),
# since they usually don't have the required options.
# Also try using 'gtime' if it exists.
ifeq (OK,$(shell env time -f 'test' -o '/dev/null' echo OK 2>/dev/null || echo KO))
define time_with_output
  env time -f 'user_time=%U\nmemory=%M' -o "$(1)"
endef
else
ifeq (OK,$(shell gtime -f 'test' -o '/dev/null' echo OK 2>/dev/null || echo KO))
define time_with_output
  gtime -f 'user_time=%U\nmemory=%M' -o "$(1)"
endef
define time_with_output
endef
endif
endif

# --- Utilities ---

# Note: the 'Command: ...' line below is awfully complex due to the fact that
# arguments with quotes (e.g. CPPFLAGS) need to be properly escaped to avoid
# syntax errors in exotic cases, and we want to preserve quotes so the user
# can simply copy-paste the line printed in the terminal during execution.
ifeq ($(SILENT),yes)
define display_command
endef
else
define display_command
  @{
    echo '';
    [ -t 1 ] && tput setaf 4;
    echo 'Command: $(subst ','"'"',$(subst \,\\,$(strip $(1))))';
    [ -t 1 ] && tput sgr0;
    echo '';
  }
endef
endif

empty :=
space := $(empty) $(empty)
comma := ,

fc_list = $(subst $(space),$(comma),$(strip $1))


# --- Default configuration ---

FRAMAC     ?= frama-c
FRAMAC_SCRIPT = $(FRAMAC)-script
FRAMAC_GUI ?= frama-c-gui
EVAFLAGS   ?= \
  -eva-no-show-progress -eva-msg-key=-initial-state,-final-states,callstacks \
  -eva-warn-key alarm=inactive \
  -eva-warn-key garbled-mix=warning,garbled-mix:write=warning \
  -calldeps -from-verbose 0 \
	-cache-size 8 \
  $(if $(EVABUILTINS), -eva-builtin=$(call fc_list,$(EVABUILTINS)),) \
  $(if $(EVAUSESPECS), -eva-use-spec $(call fc_list,$(EVAUSESPECS)),)
WPFLAGS    ?=
PARSEFLAGS ?=
FCFLAGS    ?=
IVETTEFLAGS ?=
FCGUIFLAGS ?= $(IVETTEFLAGS)

export LIBOVERLAY_SCROLLBAR=0


# --- Cleaning ---

.PHONY: clean
clean::
	$(RM) -r *.parse *.eva

clean-backups:
	find . -regextype posix-extended \
	  -regex '^.*_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}\.eva(\.(log|stats|alarms|warnings|metrics))?' \
	  -delete

# --- Generic rules ---

HR_TIMESTAMP := $(shell date +"%H:%M:%S %d/%m/%Y")# Human readable
DIR          := $(dir $(lastword $(MAKEFILE_LIST)))
SHELL        := $(shell which bash)
.SHELLFLAGS  := -eu -o pipefail -c

.ONESHELL:
.SECONDEXPANSION:
.FORCE:
.SUFFIXES: # Disable make builtins

%.parse/command %.eva/command %.wp/command:
	@#

%.parse: SOURCES = $(filter-out %/command,$^)
%.parse: PARSE = $(FRAMAC) \
                 $(PARSEFLAGS) \
                 $(FCFLAGS) \
                 $(if $(value MACHDEP),-machdep $(MACHDEP),) \
                 -cpp-extra-args="$(CPPFLAGS)" \
                 $(if $(findstring -mopsa-db,$(PARSEFLAGS)),,$(SOURCES)) \

%.parse: $$(if $$^,,.IMPOSSIBLE) $$(shell $(SHELL) $(DIR)cmd-dep.sh $$@/command $$(PARSE))
	@$(call display_command,$(PARSE))
	mkdir -p $@
	# Beware: currently, we perform some extra operations
	# (load/remove projects/save) that can affect benchmarking!
	$(if $(AST_DIFF),\
	  $(if $(wildcard $*.eva/framac.sav), \
               $(FRAMAC) $(FCFLAGS) -load $*.eva/framac.sav \
               -remove-projects @all_but_current -save $@/framac.reparse; \
               rm $*.eva/framac.sav,\
               $(if $(wildcard $@/framac.sav), \
                    mv $@/framac.sav $@/framac.reparse,true)),\
          true)
	mv -f $@/{command,running}
	{
	  $(call time_with_output,$@/stats.txt) \
	    $(PARSE) \
	      -kernel-log w:$@/warnings.log \
	      -metrics -metrics-log a:$@/metrics.log \
	      -save $@/framac.sav \
	      -print -ocode $@/framac.ast -then -no-print \
	    || ($(RM) -r $@; false) # Ensures target failure
	} 2>&1 |
	  $(SED_UNBUFFERED) '/\[metrics/,999999d' |
	  tee $@/parse.log
	{
	  printf 'timestamp=%q\n' "$(HR_TIMESTAMP)";
	  printf 'warnings=%s\n' "`cat $@/warnings.log | grep 'Warning:' | wc -l`";
	  printf 'cmd_args=%q\n' "$(subst ",\",$(wordlist 2,999,$(PARSE)))"
	} >> $@/stats.txt
	mv $@/{running,command}
	touch $@ # Update timestamp and prevent remake if nothing changes

define incremental
  $(if $(AST_DIFF),\
    $(if $(wildcard $@/framac.sav),\
      -eva-load $@/framac.sav,\
      $(warning Cannot do incremental analysis: no previously saved state)))
endef

%.eva: EVA = $(FRAMAC) $(FCFLAGS) -eva $(call incremental,$1) $(EVAFLAGS)
%.eva: PARSE_RESULT = $(word 1,$(subst ., ,$*)).parse
%.eva: $$(PARSE_RESULT) $$(shell $(SHELL) $(DIR)cmd-dep.sh $$@/command $$(EVA)) $(if $(BENCHMARK),.FORCE,)
	@$(call display_command,$(EVA))
	mkdir -p $@
	mv -f $@/{command,running}
	{
	  $(call time_with_output,$@/stats.txt) \
	    $(EVA) \
	      -load $(PARSE_RESULT)/framac.sav \
	      -eva-flamegraph $@/flamegraph.txt \
	      -kernel-log w:$@/warnings.log \
	      -from-log w:$@/warnings.log \
	      -inout-log w:$@/warnings.log \
	      -scope-log w:$@/warnings.log \
	      -eva-log w:$@/warnings.log \
	      -eva-statistics-file $@/eva-stats.csv \
	      -save $@/framac.sav \
	      -then \
	      -report-csv $@/alarms.csv -report-no-proven \
	      -report-log w:$@/warnings.log \
	      -metrics-eva-cover \
	      -metrics-log a:$@/metrics.log \
	      -nonterm -nonterm-log a:$@/nonterm.log \
	    || (mv -f $@/{running,command} &&
	        $(RM) $@/stats.txt &&
	        false) # Prevents having error code reporting in stats.txt
	} 2>&1 |
	  tee $@/eva.log
	$(SHELL) $(DIR)parse-coverage.sh $@/eva.log $@/stats.txt
	{
	  printf 'timestamp=%q\n' "$(HR_TIMESTAMP)";
	  printf 'warnings=%s\n' "`cat $@/warnings.log | grep 'Warning:' | wc -l`";
	  printf 'alarms=%s\n' "`expr $$(cat $@/alarms.csv | wc -l) - 1`";
	  printf 'cmd_args=%q\n' "$(subst ",\",$(wordlist 2,999,$(EVA)))";
	  printf 'benchmark_tag=%s' "$(BENCHMARK)"
	} >> $@/stats.txt
	if [ ! -z $${FLAMEGRAPH+x} ]; then
	  NOGUI=1 $(FRAMAC_SCRIPT) flamegraph $@/flamegraph.txt $@/
	fi
	mv $@/{running,command}
	touch $@ # Update timestamp and prevents remake if nothing changes

%.wp: WP = $(FRAMAC) $(FCFLAGS) -wp $(WPFLAGS)
%.wp: PARSE_RESULT = $(word 1,$(subst ., ,$*)).parse
%.wp: $$(PARSE_RESULT) $$(shell $(SHELL) $(DIR)cmd-dep.sh $$@/command $$(WP)) $(if $(BENCHMARK),.FORCE,)
	@$(call display_command,$(WP))
	mkdir -p $@
	mv -f $@/{command,running}
	{
	  $(call time_with_output,$@/stats.txt) \
	    $(WP) \
	      -load $(PARSE_RESULT)/framac.sav -save $@/framac.sav \
	      -kernel-log w:$@/warnings.log \
	      -wp-log w:$@/warnings.log \
	      -then \
	      -report-csv $@/alarms.csv -report-no-proven \
	      -report-log w:$@/warnings.log \
	    || (mv -f $@/{running,command} &&
	        $(RM) $@/stats.txt &&
	        false) # Prevents having error code reporting in stats.txt
	} 2>&1 |
	  tee $@/wp.log
	{
	  printf 'timestamp=%q\n' "$(HR_TIMESTAMP)";
	  printf 'warnings=%s\n' "`cat $@/warnings.log | grep 'Warning:' | wc -l`";
	  printf 'alarms=%s\n' "`expr $$(cat $@/alarms.csv | wc -l) - 1`";
	  printf 'cmd_args=%q\n' "$(subst ",\",$(wordlist 2,999,$(WP)))";
	  printf 'benchmark_tag=%s' "$(BENCHMARK)"
	} >> $@/stats.txt
	mv $@/{running,command}
	touch $@ # Update timestamp and prevent remake if nothing changes

%.gui: %
	$(FRAMAC_GUI) $(FCGUIFLAGS) -load $^/framac.sav &

%.ivette: %
	$(warning The .ivette target is deprecated, use .gui)
	$(FRAMAC_GUI) $(FCGUIFLAGS) -load $^/framac.sav &

# Produce and open an SVG + HTML from raw flamegraph data produced by Eva
%/flamegraph: %/flamegraph.html
	@
	case "$$OSTYPE" in
	  cygwin*) cmd /c start "$^";;
	  linux*) xdg-open "$^";;
	  darwin*) open "$^";;
	esac

%/flamegraph.html %/flamegraph.svg: %/flamegraph.txt
	NOGUI=1 $(FRAMAC_SCRIPT) flamegraph $^ $(dir $^)

.PRECIOUS: %/flamegraph.html

# clean is generally not the default goal, but if there is no default
# rule when including this file, it would be.

ifeq ($(.DEFAULT_GOAL),clean)
  .DEFAULT_GOAL :=
endif
