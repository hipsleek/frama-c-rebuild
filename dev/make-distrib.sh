#!/usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

set -e

################################################################################
# Configuration

if [ -z ${HDRCK+x} ]; then
  HDRCK="dune exec --no-print-directory --root tools/hdrck -- frama-c-hdrck"
fi

if [ -z ${VERSION_CODENAME+x} ]; then
  VERSION_CODENAME=$(cat VERSION_CODENAME)
fi

if [ -z ${CI_LINK+x} ]; then
  CI_LINK="no"
fi

# For macOS: use gtar if available, otherwise test if tar is BSD
if command -v gtar &> /dev/null; then
  TAR=gtar
else
  if tar --version | grep -q bsdtar; then
    echo "GNU tar required"
    exit 1
  fi
  TAR=tar
fi


################################################################################
# Command Line

while [ "$1" != "" ]
do
    case "$1" in
        "-h"|"-help"|"--help")
            echo "Make Frama-C Source Distribution"
            echo ""
            echo "USAGE"
            echo ""
            echo "  ./dev/make-distrib.sh [OPTIONS]"
            echo ""
            echo "OPTIONS"
            echo ""
            echo "  --help            Print this help message"
            echo "  --ci-link         Symlink to frama-c.tar.gz"
            echo "  --hdrck <cmd>     Check headers command"
            echo "  --codename <name> Set local VERSION_CODENAME"
            echo ""
            echo "ENVIRONMENT VARIABLES"
            echo ""
            echo ""
            echo "  HDRCK=<cmd> (overridden set by --hdrck)"
            echo "  VERSION_CODENAME=<name> (overridden by --codename)"
            echo "  CI_LINK=yes|no (also set by --ci-link)"
            echo "  USE_STASH=yes|no (default: no)"
            echo ""
            exit 0
            ;;
        "--hdrck")
            shift
            HDRCK="$1"
            ;;
        "--codename")
            shift
            VERSION_CODENAME="$1"
            ;;
        "--ci-link")
            CI_LINK=yes
            ;;
        *)
            echo "Don't know what to do with option '$1'"
            exit 1
            ;;
    esac
    shift
done

################################################################################
# Target Names

VERSION=$(cat VERSION)
VERSION_SAFE="${VERSION/~/-}"

FRAMAC="frama-c-$VERSION_SAFE-$VERSION_CODENAME"
if [ "$USE_STASH" == "yes" ]; then
    FRAMAC_TAR="frama-c-current.tar"
else
    FRAMAC_TAR="$FRAMAC.tar"
fi

################################################################################
# Check Opam file

OPAM_VERSION=$(cat opam | grep "^version" | sed 's/version: \"\(.*\)\"/\1/')

if [ "$VERSION" != "$OPAM_VERSION" ]; then
  echo "VERSION ($VERSION) and OPAM_VERSION ($OPAM_VERSION) differ"
  exit 2
fi

################################################################################
# Check that no versioned file is ignored

IGNORED_FILES="$(git ls-files --ignored --exclude-standard -c)"
if [ "" != "$IGNORED_FILES" ]; then
  echo "Some versioned files are ignored by .gitignore:"
  echo "$IGNORED_FILES"
  exit 2
fi

################################################################################
# External Plugins

# (using declare and a while read loop because MacOS is still on bash 3.2 by
#  default and does not know readarray D:)

declare -a FC_PLUGINS
while IFS= read -r -d $'\0' p; do FC_PLUGINS+=("$p"); done < <(
  find src/plugins -mindepth 1 -maxdepth 1 -type d -print0)
declare -a IVETTE_PLUGINS
while IFS= read -r -d $'\0' p; do IVETTE_PLUGINS+=("$p"); done < <(
  find ivette/src/frama-c/plugins -mindepth 1 -maxdepth 1 -type d -print0)
declare -a FC_EXTERNAL_PLUGINS
while IFS= read -r -d $'\0' p; do FC_EXTERNAL_PLUGINS+=("$p"); done < <(
  find src/plugins -type d -name ".git" -print0 | sed "s/\/.git//g")
declare -a IVETTE_EXTERNAL_PLUGINS
while IFS= read -r -d $'\0' p; do IVETTE_EXTERNAL_PLUGINS+=("$p"); done < <(
  find ivette/src/frama-c/plugins -type d -name ".git" -print0 | sed "s/\/.git//g")

PLUGINS=("${FC_PLUGINS[@]}" "${IVETTE_PLUGINS[@]}")
EXTERNAL_PLUGINS=("${FC_EXTERNAL_PLUGINS[@]}" "${IVETTE_EXTERNAL_PLUGINS[@]}")

################################################################################
# Summary

echo "----------------------------------------------------------------"
echo "Make Distribution"
echo "Version: $VERSION ($VERSION_CODENAME)"
echo "Frama-C Plug-ins:"
if [ "${#FC_EXTERNAL_PLUGINS[@]}" -gt 0 ]; then
  printf " * %s\n" "${FC_EXTERNAL_PLUGINS[@]}"
fi
echo "Ivette Plug-ins:"
if [ "${#IVETTE_EXTERNAL_PLUGINS[@]}" -gt 0 ]; then
  printf " * %s\n" "${IVETTE_EXTERNAL_PLUGINS[@]}"
fi
echo "----------------------------------------------------------------"

################################################################################
# Warn if there are uncommitted changes (will not be taken into account)

# We do want word splitting, disable SC2046
# shellcheck disable=SC2046
GIT_STATUS="$(git status --porcelain -- $(printf ":!%s\n" "${EXTERNAL_PLUGINS[@]}"))"
if [ "" != "$GIT_STATUS" ] && [ "$USE_STASH" != "yes" ]; then
  echo "WARNING: uncommitted changes will be IGNORED when making archive:"
  # Unable to replace sed with ${var//search/replace} here, disable SC2001
  # shellcheck disable=SC2001
  echo "$GIT_STATUS" | sed 's/^/  /'
  echo "----------------------------------------------------------------"
fi

################################################################################
# Prepare Archive

# For the "instant Docker image" script: allow inclusion of uncommitted changes
if [ "$USE_STASH" == "yes" ]; then
    ARCHIVE_COMMIT=$(git stash create)
fi

git archive "${ARCHIVE_COMMIT:-HEAD}" -o "$FRAMAC_TAR" --prefix "$FRAMAC/"

################################################################################
# Add external plugins to archive

if [ "${#EXTERNAL_PLUGINS[@]}" -gt 0 ]
then
  echo "Including external plugins:"
fi

for plugin in "${EXTERNAL_PLUGINS[@]}"
do
    echo "  $plugin"
    PLUGIN_TAR="$(basename "$plugin").tar"
    git -C "$plugin" archive HEAD -o "$PLUGIN_TAR" --prefix "$FRAMAC/$plugin/"
    $TAR --concatenate --file="$FRAMAC_TAR" "$plugin/$PLUGIN_TAR"
    rm -rf "${plugin:?}/${PLUGIN_TAR:?}"
done

if [ "${#EXTERNAL_PLUGINS[@]}" -gt 0 ]
then
  echo "----------------------------------------------------------------"
fi


################################################################################
# Prepare header spec

echo "Preparing headers..."

HEADER_SPEC="header-spec.txt"

git ls-files |\
git check-attr --stdin export-ignore |\
grep -v "export-ignore: set" | awk -F ': ' '{print $1}' |\
git check-attr --stdin header_spec > "$HEADER_SPEC"

for plugin in "${EXTERNAL_PLUGINS[@]}" ; do
  git -C "$plugin" ls-files |\
  git -C "$plugin" check-attr --stdin export-ignore |\
  grep -v "export-ignore: set" | awk -F ': ' '{print $1}' |\
  git -C "$plugin" check-attr --stdin header_spec |\
  xargs -n3 printf "$plugin/%s %s %s\n" >> "$HEADER_SPEC"
done

################################################################################
# Build option for check

CHECK_HEADER_OPT="-header-dirs headers"

# For plugins, either they can be open-source and we assume they have OS headers
# or they are closed-source
for plugin in "${PLUGINS[@]}" ; do
  if [ -d "$plugin/headers" ] ; then
    CHECK_HEADER_OPT="$CHECK_HEADER_OPT -header-dirs $plugin/headers"
  fi
done

################################################################################
# Headers

echo "Check headers..."

TMP_DIR=$(mktemp -d)
$TAR xf "$FRAMAC_TAR" -C "$TMP_DIR"

# Check
# We do want globbing and word splitting, disable SC2086
# shellcheck disable=SC2086
$HDRCK $CHECK_HEADER_OPT -spec-format="3-fields-by-line" -C "$TMP_DIR/$FRAMAC" $HEADER_SPEC

################################################################################
# Finalize archive

echo "Finalizing archive..."

echo "$VERSION_SAFE" > "$TMP_DIR/$FRAMAC/VERSION"
echo "$VERSION_CODENAME" > "$TMP_DIR/$FRAMAC/VERSION_CODENAME"

DATE="$(date +%F)"

$TAR czf "$FRAMAC_TAR.gz" -C "$TMP_DIR" "$FRAMAC" \
  --numeric-owner --owner=0 --group=0 --sort=name --mode='a+rw' \
  --mtime="$DATE Z"

if [[ "$CI_LINK" == "yes" ]]; then
  ln "$FRAMAC_TAR.gz" "frama-c.tar.gz"
fi

################################################################################
# Cleaning

rm -rf "$HEADER_SPEC"
rm -rf "$FRAMAC_TAR"
rm -rf "$TMP_DIR"

echo "Generated: $FRAMAC_TAR.gz"
echo "----------------------------------------------------------------"
