# Produces a self-contained binary for 'frama-c-script estimate-difficulty'
# on a machine without Frama-C.

# Example usage:
# export FRAMAC=$(which frama-c) && make -f fc-estimate-difficulty.mk fc-estimate-difficulty
# export FRAMAC=$(which frama-c) && make -f fc-estimate-difficulty.mk fc-estimate-difficulty.exe

# Works on Linux and macOS. The Windows binary is compiled and tested using
# a Docker image with wine and pyinstaller (usage of Docker avoids issues with
# staging versions in e.g. Fedora, which may cause weird error messages when
# running in Windows). The Linux binary is tested on a bare-bones Alpine Linux,
# to ensure it does not depend on dynamic libraries.
# The macOS version needs to be tested by hand.

# Notes:
# pip install "staticx==0.14.1"
# pip install "patchelf==0.17.0.0" # seems to work; version 0.17.2.1 did not work when tried

help:
	@echo "targets:"
	@echo "fc-estimate-difficulty           : Linux/macOS version (depending on host system)"
	@echo "fc-estimate-difficulty.exe       : Windows cross-compiled version (from Linux)"
	@echo "clean                            : Erase working directory"
	@echo "distclean                        : Clean and erase generated files"
	@echo "test-fc-estimate-difficulty      : Test Linux/macOS version"
	@echo "test-fc-estimate-difficulty.exe  : Test Windows version"

os := $(shell uname -s)

workdir = fced-dist-prepare
distdir = fced-dist

# if 'podman' is installed, we assume it is preferred over Docker;
# otherwise, try using 'docker'
ifneq (, $(shell which podman))
DOCKER:=podman
else
DOCKER:=docker
endif

libc_metrics.json: ../libc/__fc_libc.h ../libc/__fc_runtime.c
ifeq ($(wildcard $(FRAMAC)),)
	$(error FRAMAC must be set to the path of the 'frama-c' binary)
endif
	FRAMAC_SHARE="$(shell $(FRAMAC) -no-autoload-plugins -print-share-path)"
	rm -f $@
	$(FRAMAC) \
	  $^ \
	  -no-autoload-plugins -load-module metrics \
	  -metrics -metrics-libc -metrics-output $@
	chmod -w $@ # generated file: prevent accidental overwriting

PY_DEPS := \
  build_callgraph.py \
  estimate_difficulty.py \
  external_tool.py \
  fclog.py \
  function_finder.py \
  source_filter.py \

TOOLS := $(workdir)/scc $(workdir)/astyle
COMMON_DEPS := $(PY_DEPS) libc_metrics.json

fc-estimate-difficulty: $(COMMON_DEPS) $(TOOLS)
fc-estimate-difficulty.exe: \
  $(COMMON_DEPS) $(addsuffix .exe,$(TOOLS)) fced-win.Dockerfile

fc-estimate-difficulty:
	pyinstaller estimate_difficulty.py \
	  -F \
	  -n $@ \
	  --distpath $(distdir) \
	  --noconfirm \
	  --add-data "libc_metrics.json:share" \
	  --add-binary "$(workdir)/scc:." \
	  --add-binary "$(workdir)/astyle:."
ifeq ($(os),Linux)
	@echo "Linux: running staticx"
	staticx "$(distdir)/$@" $@
else
	@echo "NOT running staticx (macOS?)"
	mv "$(distdir)/$@" $@
endif

fc-estimate-difficulty.exe:
	$(DOCKER) build . -f fced-win.Dockerfile -t fced-win
	$(DOCKER) cp $$($(DOCKER) create --rm fced-win):/fced/$@ $@

$(workdir)/scc: $(workdir)/scc-snapshots-fc-1.1.0
	make -C $< clean
	make -C $< scc $(if $(findstring Linux,$(os)),CFLAGS="-static") -j
	cp $(workdir)/scc-snapshots-fc-1.1.0/scc $@

$(workdir)/scc.exe: $(workdir)/scc-snapshots-fc-1.1.0
	make -C $< clean
	make -C $< scc CC=x86_64-w64-mingw32-gcc -j
	cp $(workdir)/scc-snapshots-fc-1.1.0/scc.exe $@

$(workdir)/scc-snapshots-fc-1.1.0:
	mkdir -p $(workdir)
	wget 'https://github.com/Frama-C/scc-snapshots/archive/refs/tags/fc-1.1.0.tar.gz' -O $(workdir)/fc-1.1.0.tar.gz
	cd $(workdir) && tar xvf fc-1.1.0.tar.gz
	touch $@

$(workdir)/astyle: $(workdir)/astyle_3.1
	make -C $(workdir)/astyle_3.1/build/gcc clean
	make -C $(workdir)/astyle_3.1/build/gcc \
	  $(if $(findstring Linux,$(os)),LDFLAGS="-static" CFLAGS="-static -static-libgcc -static-libstdc++") -j
	cp $(workdir)/astyle_3.1/build/gcc/bin/astyle $@

$(workdir)/astyle.exe: $(workdir)/astyle_3.1
	make -C $(workdir)/astyle_3.1/build/gcc clean
	make -C $(workdir)/astyle_3.1/build/gcc LDFLAGS="-static" CFLAGS="-static -static-libgcc -static-libstdc++" CXX=x86_64-w64-mingw32-g++ -j
	cp $(workdir)/astyle_3.1/build/gcc/bin/astyle.exe $@

$(workdir)/astyle_3.1:
	mkdir -p $(workdir)
	wget 'https://downloads.sourceforge.net/project/astyle/astyle/astyle%203.1/astyle_3.1_linux.tar.gz' -O $(workdir)/astyle_3.1_linux.tar.gz
	cd $(workdir) && tar xvf astyle_3.1_linux.tar.gz
	rm -rf $(workdir)/astyle_3.1
	mv $(workdir)/astyle $(workdir)/astyle_3.1
	sed -i.bak 's/^CXX = g++/CXX ?= g++/' $(workdir)/astyle_3.1/build/gcc/Makefile
	sed -i.bak 's/^CXX = g++/CXX ?= g++/' $(workdir)/astyle_3.1/build/gcc/Makefile
	touch $@

clean:
	rm -rf build $(workdir) $(distdir) fc-estimate-difficulty.spec fc-estimate-difficulty.exe.spec

distclean: clean
	rm -f fc-estimate-difficulty fc-estimate-difficulty.exe libc_metrics.json

test-fc-estimate-difficulty: fc-estimate-difficulty
ifeq ($(os),Linux)
	$(DOCKER) build . -f fced-lin.Dockerfile -t fced-lin
else
	./$< fced-test | \
	  grep -A9 "Overall difficulty score" | \
	  grep -v 0 | \
	  grep -q ': '
endif
	@echo "Done testing $^."

test-fc-estimate-difficulty.exe: fc-estimate-difficulty.exe
	@file $< | grep -q "MS Windows"
	@echo "Done testing $< (actual testing done inside Docker image)"

.PHONY: clean distclean test-fc-estimate-difficulty test-fc-estimate-difficulty.exe
