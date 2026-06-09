##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# --------------------------------------------------------------------------
APP=Frama-C GUI
DOME=./src/dome
DOME_ARGS=--command $$(dirname $$0)/frama-c
DOME_DEV=
DOME_CLI=../bin/frama-c-gui
DOME_API=./src/frama-c
DOME_CUSTOM_ENTRIES= yes
COPYRIGHT=CEA LIST / LSL
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# --- Ivette Compilation
# --------------------------------------------------------------------------

.PHONY: all app dev clean

all: api pkg lint dome-app
app: api dome-app
dev: api dome-dev

# --------------------------------------------------------------------------
# --- Ivette Linting
# --------------------------------------------------------------------------

.PHONY: lint check-lint check-case

# With autofix
lint: dome-pkg dome-templ check-case
	@echo "[Ivette] running typechecker & linter (with cache & fix mode)"
	yarn run typecheck
	yarn run lint --fix --cache --cache-location .eslint-cache

# Without autofix
check-lint: dome-pkg dome-templ check-case
	@echo "[Ivette] running typechecker & linter"
	yarn run typecheck
	yarn run lint

# In case-insensitive filesystems (macOS/Windows), import statements and
# filenames can have issues due to case differences
check-case:
	@echo "[Ivette] check case issues in imported filenames"
	@err=0 ; for f in `find src | sed -E 's/(.ts|.tsx|.js|.jsx|.css|.json)$$//' | sort -f | uniq -di` ;\
	 do \
		err=1; \
		echo " - ambiguous $$f.*"; \
	 done ;\
	 exit $$err

# --------------------------------------------------------------------------
# --- Help
# --------------------------------------------------------------------------

help::
	@echo "Ivette installation configuration variables"
	@echo "  - PREFIX: used to customize installation path"

# --------------------------------------------------------------------------
# --- Ivette Distribution
# --------------------------------------------------------------------------

.PHONY: dist appimage dist-dir dist-windows dist-macOS dist-linux

dist: dist-dir dome-dist
dist-linux: dist-dir dome-appimage
dist-windows: dist-dir dome-dist-win
dist-macOS: dist-dir dome-dist-mac

dist-dir:
	@echo "Cleaning dist"
	@rm -fr dist

# --------------------------------------------------------------------------
# --- Ivette Package Loader
# --------------------------------------------------------------------------

.PHONY: pkg

LOADER=src/renderer/loader.ts
SANDBOX=src/renderer/sandbox.ts
PACKAGES=$(shell find src -name "pkg.json")
SANDBOXES=$(shell find src/sandbox -name "*.tsx")

dome-pkg: pkg
dome-app: pkg
dome-dev: pkg
dome-dist: pkg
dome-appimage: pkg
dome-dist-win: pkg
dome-dist-mac: pkg

pkg: $(LOADER) $(SANDBOX)

$(LOADER): $(PACKAGES) ./configure.js ./Makefile
	@rm -f $@
	@echo "[Ivette] configure packages"
	@node ./configure.js $@ $(PACKAGES)
	@chmod -f a-w $@

$(SANDBOX): $(SANDBOXES) ./sandboxer.js ./Makefile
	@rm -f $@
	@echo "[Ivette] configure sandboxes"
	@node ./sandboxer.js $@ $(SANDBOXES)
	@chmod -f a-w $@

# --------------------------------------------------------------------------
# --- Frama-C API
# --------------------------------------------------------------------------

.PHONY: api

api:
	@echo "[Ivette] Generating TypeScript API"
	./api.sh src

# --------------------------------------------------------------------------
# --- Ivette Documentation
# --------------------------------------------------------------------------

.PHONY: icons

VERSION=$(shell echo "console.log(require('./package.json').version)" | node -)

DOC_ICONS_CSS= src/dome/doc/gallery.css
DOC_ICONS_MAKE= src/dome/doc/iconsmd.js
DOC_ICONS_GALLERY= src/dome/doc/guides/icons.md
DOC_ICONS_DATA= src/dome/renderer/controls/gallery.json
DOC_GUIDES= src/dome/doc/guides

NODEBIN= ./node_modules/.bin

DOCDIRS= src/ivette src/dome/renderer src/dome/misc src/frama-c/kernel/api src/frama-c/plugins/*/api
ENTRYPOINTS= $(addprefix -entryPoints , $(DOCDIRS))
DOCFILES= src/frama-c/server.ts src/frama-c/states.ts --entryPointStrategy expand $(ENTRYPOINTS)

icons: $(DOC_ICONS_GALLERY)

$(DOC_ICONS_GALLERY): $(DOC_ICONS_MAKE) $(DOC_ICONS_DATA)
	@rm -f $@
	@node $(DOC_ICONS_MAKE) $(DOC_ICONS_DATA) > src/dome/doc/guides/icons.md
	@chmod a-w $@

doc: $(NODEBIN)/typedoc icons
	@echo "\n[Ivette] documentation"
	@rm -fr doc/html
	@yarn run typedoc $(DOCFILES)
	@echo "\n[Ivette] main page"
	@echo ""
	@echo "   file://$(PWD)/doc/html/index.html"
	@echo ""

$(NODEBIN)/%:
	@echo "[Node] install package $*"
	yarn add -D $*

# --------------------------------------------------------------------------
include $(DOME)/template/makefile
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# --- Cleaning Development Files
# --------------------------------------------------------------------------

clean:
	@rm -f yarn-error.log
	@rm -f .eslint-cache
	@find . -name ".dome-*.stamp" -delete
	@find . -name ".dome-*.back" -delete
	@rm -fr node_modules
	@rm -fr dist
	@rm -fr out
	@rm -fr doc/html
	@rm -f src/renderer/loader.ts
	@rm -f src/renderer/sandbox.ts
	@rm -fr src/renderer/public
	@rm -f src/dome/doc/guides/icons.md
	@rm -f icon.icns
	@rm -fr src/frama-c/kernel/api
	@rm -fr src/frama-c/plugins/*/api

# --------------------------------------------------------------------------
# --- Ivette Installation
# --------------------------------------------------------------------------

.PHONY: install

BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib

UNAME=$(shell uname)

ifeq ($(UNAME),Darwin)

dome-dist: icon.icns
icon.icns: static/icon.png
	rm -fr icon.iconset
	mkdir icon.iconset
	sips -z 16 16    static/icon.png --out "icon.iconset/icon_16x16.png"
	sips -z 32 32    static/icon.png --out "icon.iconset/icon_16x16@2x.png"
	sips -z 32 32    static/icon.png --out "icon.iconset/icon_32x32.png"
	sips -z 64 64    static/icon.png --out "icon.iconset/icon_32x32@2x.png"
	sips -z 128 128  static/icon.png --out "icon.iconset/icon_128x128.png"
	sips -z 256 256  static/icon.png --out "icon.iconset/icon_128x128@2x.png"
	sips -z 256 256  static/icon.png --out "icon.iconset/icon_256x256.png"
	sips -z 512 512  static/icon.png --out "icon.iconset/icon_256x256@2x.png"
	sips -z 512 512  static/icon.png --out "icon.iconset/icon_512x512.png"
	iconutil -c icns icon.iconset
	rm -fr icon.iconset

endif

IVETTE_DIST=$(wildcard dist/linux-unpacked dist/mac dist/mac-arm64)

ifdef PREFIX
ifeq ($(IVETTE_DIST),dist/linux-unpacked)

install:
	@echo "Installing Ivette (Linux)"
	@mkdir -p $(LIBDIR)/frama-c
	@rm -fr $(LIBDIR)/frama-c/ivette
	@mv $(IVETTE_DIST) $(LIBDIR)/frama-c/ivette
	@mkdir -p $(BINDIR)
	@rm -f $(BINDIR)/frama-c-gui
	@ln -s $(LIBDIR)/frama-c/ivette/frama-c-gui $(BINDIR)/frama-c-gui

else ifeq ($(IVETTE_DIST:-arm64=),dist/mac)

install:
	@echo "Installing Ivette (macOS)"
	@mkdir -p $(LIBDIR)/frama-c
	@rm -fr $(LIBDIR)/frama-c/Ivette.app
	@mv $(IVETTE_DIST)/Ivette.app $(LIBDIR)/frama-c/
	@cp ivette-macos.sh dist/ivette.sh
	@echo "exec open -na $(LIBDIR)/frama-c/Ivette.app --args\\" >> dist/ivette.sh
	@echo "  --command $(BINDIR)/frama-c\\" >> dist/ivette.sh
	@echo "  --working \$$PWD \"\$$@\"" >> dist/ivette.sh
	@mkdir -p $(BINDIR)
	@install dist/ivette.sh $(BINDIR)/frama-c-gui
	@rm -fr $(IVETTE_DIST)

else

install:
	@echo "No distribution to install"
	@echo "use 'make dist' and retry"
	@exit 1

endif
else

install:
	@echo "PREFIX not defined"
	@exit 1

endif

# --------------------------------------------------------------------------
# --- Ivette Un-Installation
# --------------------------------------------------------------------------

ifdef PREFIX

uninstall:
	@echo "Uninstall Ivette"
	@rm -f  $(BINDIR)/frama-c-gui
	@rm -f  $(LIBDIR)/frama-c/ivette.tgz
	@rm -fr $(LIBDIR)/frama-c/Ivette.app
	@rm -fr $(LIBDIR)/frama-c/ivette

endif

# --------------------------------------------------------------------------
# --- Ivette Tests
# --------------------------------------------------------------------------

tests: dist
	@echo "[Ivette] running tests"
	@dune exec -- yarn playwright test

# --------------------------------------------------------------------------
