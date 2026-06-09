##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

.ONESHELL:
.PHONY: dev* custom* help push

help:
	@echo "Available targets are:"
	echo "- dev.<distro>: public Git development version"
	echo "- custom.<distro>: version based on a frama-c-*.tar.gz archive"
	echo "  (custom targets require FRAMAC_ARCHIVE to be set)"
	echo "- <version>-all: applies to all distributions"
	echo ""
	echo "Currently, three distributions are supported:"
	echo "- debian-13 (default, i.e. '<v>.debian' is aliased to '<v>')"
	echo "- fedora-42"
	echo "- alpine-3.22 (note: E-ACSL disabled due to musl)"
	echo
	echo "Base images are ocaml/opam:<distro>-ocaml-<version>"
	echo "(where <version> is based on Frama-C's reference-configuration.md)"
	echo ""
	echo "Targets for users with write access to the framac Docker Hub:"
	echo "- push TAG=<tag1> AS=<tag2>: perform a docker push with image"
	echo "  <tag1> sent as <tag2>. Usage example:"
	echo "  make push TAG=framac/frama-c:dev.debian AS=framac/frama-c:dev"
	echo ""
	echo "- dev-push-all: push all variants of dev images"
	echo ""
	echo "- custom-push-all: push all variants of custom archive images"
	echo "  (requires FRAMAC_ARCHIVE to be set)"

DEPS=../../reference-configuration.md Dockerfile $(wildcard *.sh)

%.debian: BUILD_ARGS += --build-arg=DISTRO=debian-13
%.fedora: BUILD_ARGS += --build-arg=DISTRO=fedora-42
%.alpine: BUILD_ARGS += --build-arg=DISTRO=alpine-3.22

version_short=$(shell grep -o "^- OCaml [0-9.]\+" "../../reference-configuration.md" | grep -o "[0-9][.][0-9]\+")
BUILD_ARGS += --build-arg=OCAML_VERSION_SHORT=$(version_short)
BUILD_ARGS += --build-arg=OPAM=2.4

dev-all: dev.debian \
         dev.fedora \
         dev.alpine

dev.%: $(DEPS)
	cp ../../reference-configuration.md .
	docker build . -t framac/frama-c:$@ $(BUILD_ARGS) --target frama-c

ifeq ($(FRAMAC_ARCHIVE),)
custom.%:
	@echo "error: run '$(MAKE) $@ FRAMAC_ARCHIVE=frama-c-<version>.tar.gz',"
	echo "       where frama-c-<version>.tar.gz is the name of the archive."
	echo "Note: the archive must be in the current directory and be named as"
	echo "      'frama-c-<version>.tar.gz' (where <version> is at your choice)."
	exit 1
else
BUILD_ARGS += --build-arg=from_archive=$(FRAMAC_ARCHIVE)

custom.%: $(DEPS)
	cp ../../reference-configuration.md .
	docker build . -t frama-c-$@ $(BUILD_ARGS) --target frama-c

custom-all: custom.debian \
            custom.fedora \
            custom.alpine

endif

# check if either TAG or AS are empty
ifeq ($(TAG)$(AS),$(AS)$(TAG))
push:
	@echo "error: run '$(MAKE) $@ TAG=<orig> AS=<new>'"
	echo "        where <tag> is the current tag of the image to be pushed,"
	echo "        and <new> is the name used in the Docker Hub"
	echo "        e.g. framac/frama-c:dev"
	echo "(if you supplied identical tags, just run 'docker push $(TAG)')"
	exit 1
else
push:
	docker tag $(TAG) $(AS)
	docker push $(AS)
endif

dev-push-all:
	docker push framac/frama-c:dev.debian
	docker push framac/frama-c:dev.fedora
	docker push framac/frama-c:dev.alpine
	docker tag framac/frama-c:dev.debian framac/frama-c:dev
	docker push framac/frama-c:dev

ifeq ($(CUSTOM_VERSION),)
custom-push-all:
	@echo "error: run '$(MAKE) $@ CUSTOM_VERSION=<version>'"
	exit 1
else
ifeq ($(findstring beta,$(CUSTOM_VERSION)),beta)
custom-push-all:
	@echo "error: cannot push a beta archive"
	exit 1
else
custom-push-all:
	@echo "Copy and paste the following commands:"
	echo ""
	echo "docker tag frama-c-custom.debian framac/frama-c:$(CUSTOM_VERSION).debian"
	echo ""
	echo "docker tag frama-c-custom.fedora framac/frama-c:$(CUSTOM_VERSION).fedora"
	echo ""
	echo "docker tag frama-c-custom.alpine framac/frama-c:$(CUSTOM_VERSION).alpine"
	echo ""
	echo "docker tag framac/frama-c:$(CUSTOM_VERSION).debian framac/frama-c:$(CUSTOM_VERSION)"
	echo ""
	echo "docker push framac/frama-c:$(CUSTOM_VERSION).debian"
	echo "docker push framac/frama-c:$(CUSTOM_VERSION).fedora"
	echo "docker push framac/frama-c:$(CUSTOM_VERSION).alpine"
	echo "docker push framac/frama-c:$(CUSTOM_VERSION)"
endif
endif
