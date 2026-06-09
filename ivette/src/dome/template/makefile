##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# --------------------------------------------------------------------------
# ---  Main Makefile for building Dome applications
# --------------------------------------------------------------------------

# DOMEMK_ reserved for makefile derived variables
# CURDIR is a Makefile builtin variable

APP?=$(shell basename $(CURDIR))
COPYRIGHT?=CEA Tech

DOME_MK_NAME=$(shell echo $(APP) | tr [:upper:] [:lower:])

DOME?=dome
DOME_ARGS?=
DOME_DEV_ARGS?=
DOME_APP_ARGS?=
DOME_CLI?=./bin/$(DOME_MK_NAME)
DOME_DOC?=./doc/html
DOME_API?=

# --------------------------------------------------------------------------
# ---  Makefile Help
# --------------------------------------------------------------------------

.PHONY: dome-help

dome-help:
	@echo
	@echo "[Dome] Builtin Targets:"
	@echo "  make dome-help    This help"
	@echo "  make dome-dev     Run application in development mode"
	@echo "  make dome-app     Compile application"
	@echo "  make dome-dist    Package application for production"
	@echo "  make dome-doc     Generate Dome documentation"
	@echo "  make dome-api     Generate Application documentation"
	@echo "  make dome-pkg     Install development packages"
	@echo "  make dome-templ   Update templates"
	@echo "  make dome-reboot  Rebuild templates from scratch"
	@echo "  make dome-clean   Remove dome temporary files"
	@echo
	@echo "[Dome] Development:"
	@echo "  Dome framework  DOME         = '$(DOME)'"
	@echo "  Local command   DOME_CLI     = '$(DOME_CLI)'"
	@echo "  Local arguments DOME_ARGS    = '$(DOME_ARGS)'"
	@echo "  Extra arg.(dev) DOME_DEV     = '$(DOME_DEV)'"
	@echo
	@echo "[Dome] Documentation:"
	@echo "  Application     APP          = '$(APP)'"
	@echo "  Copyright       COPYRIGHT    = '$(COPYRIGHT)'"
	@echo "  Output dir      DOME_DOC     = '$(DOME_DOC)'"
	@echo "  Public API      DOME_API     = '$(DOME_API)'"
	@echo

# --------------------------------------------------------------------------
# ---  Application Cleaning
# --------------------------------------------------------------------------

.PHONY: dome-clean

dome-clean:
	@echo "[Dome] remove temporary files"
	@rm -fr out dist .dome-*.stamp

# --------------------------------------------------------------------------
# ---  Development Packages Init & Installation
# --------------------------------------------------------------------------

.PHONY: dome-pkg

include $(DOME)/template/makefile.packages

DOME_MK_BIN=$(CURDIR)/node_modules/.bin
DOME_MK_PKG= \
	package.json node_modules \
	$(DOME)/template/makefile.packages \
	$(DOME)/template/package.sh

node_modules:
	mkdir $@

dome-pkg: .dome-pkg.stamp
.dome-pkg.stamp: $(DOME_MK_PKG)
	@echo "[Dome] ----------------------------------------------------------------"
	@echo "[Dome] installing development packages"
	@echo "[Dome] ----------------------------------------------------------------"
	@$(DOME)/template/package.sh --dev $(DOME_DEV_PACKAGES)
	@echo "[Dome] ----------------------------------------------------------------"
	@echo "[Dome] installing application packages"
	@echo "[Dome] ----------------------------------------------------------------"
	@$(DOME)/template/package.sh --app $(DOME_APP_PACKAGES)
	@echo "[Dome] ----------------------------------------------------------------"
	@echo "[Dome] building application dependencies"
	@echo "[Dome] ----------------------------------------------------------------"
	yarn install
	cp \
	  ./src/dome/template/react-virtualized.hacked.onScroll.js \
	  ./node_modules/react-virtualized/dist/es/WindowScroller/utils/onScroll.js
	yarn run electron-builder install-app-deps
	@touch $@

package.json:
	yarn init

# --------------------------------------------------------------------------
# ---  Dome Synchronization
# --------------------------------------------------------------------------

ifneq ("$(DOME)",".")

.PHONY: dome-pull dome-push

dome-pull:
	@echo "[Dome] ----------------------------------------------------------------"
	@echo "[Dome] Synchronize with updates"
	@echo "[Dome] ----------------------------------------------------------------"
	@(cd $(DOME) && ./template/dome-pull.sh)

dome-push:
	@echo "[Dome] ----------------------------------------------------------------"
	@echo "[Dome] Publishing Dome updates to feature/app/$(DOME_MK_NAME)"
	@echo "[Dome] ----------------------------------------------------------------"
	@(cd $(DOME) && ./template/dome-push.sh feature/app/$(DOME_MK_NAME))

endif

# --------------------------------------------------------------------------
# ---  Application Templating
# --------------------------------------------------------------------------

.PHONY: dome-templ dome-reboot

DOME_MK_TEMPLATES= \
	src/main/index.ts \
	src/renderer/index.ts \
	src/renderer/index.html

dome-templ: $(DOME_MK_TEMPLATES)
dome-reboot:
	echo "[Dome] Backup templates"
	@for f in $(DOME_MK_TEMPLATES) ; do mv -fv $$f $$f.backup ; done
	$(MAKE) dome-templ dome-pkg

# Templated files (*.templ is used to prevent tsc from typechecking templates)

src/renderer/index.html: $(DOME)/template/update.sh $(DOME)/template/index.html
	@$^ $@

src/renderer/index.ts: $(DOME)/template/update.sh $(DOME)/template/renderer.ts.templ
	@$^ $@

DOME_MK_LOCAL= $(DOME)/template/main.ts.local

src/main/index.ts: $(DOME)/template/update.sh $(DOME)/template/main.ts.templ
	@cp $(DOME)/template/main.ts.templ    $(DOME_MK_LOCAL)
	@echo "Dome.setName('$(APP)');"    >> $(DOME_MK_LOCAL)
	@echo "Dome.start();"              >> $(DOME_MK_LOCAL)
	@$(DOME)/template/update.sh $(DOME)/template/main.ts.local $@
	@rm -f $(DOME_MK_LOCAL)

# --------------------------------------------------------------------------
# ---  Custom Entry Points
# --------------------------------------------------------------------------

ifndef DOME_CUSTOM_ENTRIES

dome-templ: src/renderer/Application.ts
src/renderer/Application.ts:
	@echo "[Dome] creating $@ from template"
	@mkdir -p src/renderer
	@cp -f $(DOME)/template/Application.ts.templ $@

dome-templ: src/renderer/Preferences.ts
src/renderer/Preferences.ts:
	@echo "[Dome] creating $@ from template"
	@mkdir -p src/renderer
	@cp -f $(DOME)/template/Preferences.ts.templ $@

endif

# --------------------------------------------------------------------------
# ---  Application Development
# --------------------------------------------------------------------------

DOME_MK_CLIDIR=$(dir $(DOME_CLI))

.PHONY: dome-dev

dome-dev: dome-pkg dome-templ
	@echo "[Dome] linking $(DOME_CLI)"
	@mkdir -p $(DOME_MK_CLIDIR)
	@rm -f $(DOME_CLI)
	@echo "#!/bin/sh" >> $(DOME_CLI)
	@echo "export DOME='$(DOME)'" >> $(DOME_CLI)
	@echo "export DOME_ENV='dev'" >> $(DOME_CLI)
	@echo 'WDIR=$$PWD' >> $(DOME_CLI)
	@echo 'ARGV="$(DOME_ARGS) $(DOME_DEV) $$@"' >> $(DOME_CLI)
	@echo "cd $(CURDIR)" >> $(DOME_CLI)
	@echo 'exec $(DOME_MK_BIN)/electron-vite dev -- $$WDIR $$ARGV' >> $(DOME_CLI)
	@chmod 555 $(DOME_CLI)

# --------------------------------------------------------------------------
# ---  Application Building
# --------------------------------------------------------------------------

.PHONY: dome-app

dome-app: dome-pkg dome-templ
	@echo "[Dome] compiling application (production)"
	DOME=$(DOME) DOME_ENV='app' yarn run build
	@echo "[Dome] linking $(DOME_CLI)"
	@mkdir -p $(DOME_MK_CLIDIR)
	@rm -f $(DOME_CLI)
	@echo "#!/bin/sh" >> $(DOME_CLI)
	@echo "export DOME_LOCAL=true" >> $(DOME_CLI)
	@echo 'exec $(DOME_MK_BIN)/electron $(CURDIR)/out/main/index.js $(DOME_ARGS) "$$@"' >> $(DOME_CLI)
	@chmod 555 $(DOME_CLI)

# --------------------------------------------------------------------------
# ---  Application Packaging
# --------------------------------------------------------------------------

.PHONY: dome-dist dome-appimage dome-dist-win

# Compatible with all OS for installation from source (more OS specific parameters in package.json)
dome-dist: dome-app
	@echo "[Dome] packaging application (binary)"
	DOME=$(DOME) yarn run electron-builder --dir
	@echo "[Dome] application packaged in ./dist"

# Build Linux AppImage for binary distribution (more OS specific parameters in package.json)
dome-appimage: dome-app
	@echo "[Dome] packaging application (AppImage)"
	DOME=$(DOME) yarn run electron-builder --linux
	@echo "[Dome] application packaged in ./dist"

# Compatible with Windows OS (more OS specific parameters in package.json)
dome-dist-win: dome-app
	@echo "[Dome] packaging application (windows exe)"
	DOME=$(DOME) yarn run electron-builder --win
	@echo "[Dome] application packaged in ./dist"

dome-dist-mac: dome-app
	@echo "[Dome] packaging application (macOS exe)"
	DOME=$(DOME) yarn run electron-builder --mac -c.mac.identity=null
	@echo "[Dome] application packaged in ./dist"

# --------------------------------------------------------------------------
# --- Icon gallery
# --------------------------------------------------------------------------

DOME_MK_ICON_MAKE=$(DOME)/doc/icons.js
DOME_MK_ICON_DATA=$(DOME)/renderer/controls/icons.json
DOME_MK_ICON_HTML=$(DOME)/doc/template/gallery-icons.html

$(DOME_MK_ICON_HTML): $(DOME_MK_ICON_MAKE) $(DOME_MK_ICON_DATA)
	@echo "[Dome] generating icons gallery"
	@node $(DOME_MK_ICON_MAKE) $(DOME_MK_ICON_DATA) > $(DOME_MK_ICON_HTML)

# --------------------------------------------------------------------------
# ---  Dome Documentation
# --------------------------------------------------------------------------

DOME_MK_DOC_API=$(addprefix $(CURDIR)/, $(DOME_API))
DOME_MK_DOC_OUT=$(addprefix $(CURDIR)/, $(DOME_DOC))

dome-doc: $(DOME_MK_BIN)/jsdoc $(DOME_MK_ICON_HTML)
	@echo "[Dome] $(APP) documentation"
	@mkdir -p $(DOME_DOC)
	@rm -f $(DOME_DOC)/*.html
	@rm -f $(DOME_DOC)/*.png
	@rm -f $(DOME_DOC)/*.css
	@rm -fr $(DOME_DOC)/fonts
	@rm -fr $(DOME_DOC)/styles
	@(cd $(DOME) && DOME="." DOME_PWD=$(CURDIR) APP=$(APP) COPYRIGHT="$(COPYRIGHT)" \
	  $(DOME_MK_BIN)/jsdoc -c ./doc/config.json $(DOME_MK_DOC_API) \
	  -d $(DOME_MK_DOC_OUT))
	@cp -f $(DOME)/doc/gallery.css $(DOME_DOC)/styles/
	@cp -f $(DOME)/doc/tutorials/*.png $(DOME_DOC)/
	@cat \
		$(DOME)/doc/template/gallery-head.html \
		$(DOME)/doc/template/gallery-icons.html \
		$(DOME)/doc/template/gallery-tail.html \
		> $(DOME_DOC)/tutorial-icons.html
	@echo "[Dome]   $(DOME_DOC)/index.html"

# --- Doc binaries ---------------------------------------------------------

$(DOME_MK_BIN)/jsdoc:
	@echo "[Dome] installing jsdoc"
	@yarn add -D jsdoc

# --------------------------------------------------------------------------
