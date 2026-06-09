#! /bin/bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

NEXT=$1

if ! test -f VERSION; then
  echo "This script must be run from Frama-C root directory"
  exit 2
fi
if test -z "$NEXT"; then
  echo "Missing argument. Usage is:"
  echo "\$ ./dev/set-version.sh [<NN.M>|dev]"
  echo "See the Release Management Documentation for an example."
  exit 2
fi

# For macOS: use gsed if available, otherwise test if sed is BSD
if command -v gsed &>/dev/null; then
  SED=gsed
else
  if sed --version 2>/dev/null | grep -q GNU; then
    SED=sed
  else
    echo "GNU sed required"
    exit 1
  fi
fi

if [[ $NEXT == "dev" ]]; then
  CURRENT=$(cat VERSION)
  CURRENT_MAJOR=$(echo "$CURRENT" | $SED -e s/\\\([0-9]*\\\).[0-9]*.*/\\1/)

  DEV_VERSION=$(($CURRENT_MAJOR+1))
  DEV_CODENAME=$(grep "$DEV_VERSION " ./doc/periodic-elements.txt | cut -d " " -f2)
  echo "Set VERSION to $DEV_VERSION~dev"
  echo "Set CODENAME to $DEV_CODENAME"
  echo "Continue? [y/N] "
  read CHOICE
  case "${CHOICE}" in
  "Y" | "y") ;;
  *) exit 1 ;;
  esac

  echo "$DEV_VERSION~dev" > VERSION
  echo "$DEV_CODENAME" > VERSION_CODENAME
  $SED -i "s/^version: .*/version: \"$DEV_VERSION~dev\"/g" opam
  $SED -i "s/^version: .*/version: \"$DEV_VERSION~dev\"/g" tools/lint/frama-c-lint.opam
  $SED -i "s/^version: .*/version: \"$DEV_VERSION~dev\"/g" tools/hdrck/frama-c-hdrck.opam
else
  NEXT_MAJOR=$(echo "$NEXT" | $SED -e s/\\\([0-9]*\\\).[0-9]*.*/\\1/)
  NEXT_MINOR=$(echo "$NEXT" | $SED -e s/[0-9]*.\\\([0-9]*\\\).*/\\1/)
  NEXT_SUFFIX=$(echo "$NEXT"| $SED -e s/[0-9]*.[0-9]*\\\(.*\\\)/\\1/)
  NEXT_CODENAME=$(grep "$NEXT_MAJOR " ./doc/periodic-elements.txt | cut -d " " -f2)

  echo "NEXT VERSION is:"
  echo "- MAJOR:    $NEXT_MAJOR"
  echo "- MINOR:    $NEXT_MINOR"
  echo "- SUFFIX:   $NEXT_SUFFIX"
  echo "- CODENAME: $NEXT_CODENAME"

  echo ""
  echo "Continue? [y/N] "
  read CHOICE
  case "${CHOICE}" in
  "Y" | "y") ;;
  *) exit 1 ;;
  esac

  # Version

  echo "$NEXT" >VERSION
  echo "$NEXT_CODENAME" >VERSION_CODENAME

  # Ivette
  $SED -i "s/^  \"version\": .*/  \"version\": \"$NEXT_MAJOR.$NEXT_MINOR.0\",/g" ivette/package.json

  # Opam files
  $SED -i "s/^version: .*/version: \"$NEXT\"/g" opam
  $SED -i "s/^version: .*/version: \"$NEXT_MAJOR.$NEXT_MINOR\"/g" tools/lint/frama-c-lint.opam
  $SED -i "s/^version: .*/version: \"$NEXT_MAJOR.$NEXT_MINOR\"/g" tools/hdrck/frama-c-hdrck.opam

  # Changelogs

  FC_CHANGELOG="Changelog"
  FC_CL_MSG_FUTURE="Open Source Release <next-release>"
  FC_CL_MSG_NEXT="Open Source Release $NEXT_MAJOR.$NEXT_MINOR ($NEXT_CODENAME)"
  FC_CL_LIN="###############################################################################"

  if ! grep -q -e " *$FC_CL_MSG_NEXT" $FC_CHANGELOG; then
      $SED -i "s/\($FC_CL_MSG_FUTURE\)/\1\n$FC_CL_LIN\n\n$FC_CL_LIN\n$FC_CL_MSG_NEXT/g" $FC_CHANGELOG;
  fi

  EA_CHANGELOG="src/plugins/e-acsl/doc/Changelog"
  EA_CL_MSG_FUTURE="Plugin E-ACSL <next-release>"
  EA_CL_MSG_NEXT="Plugin E-ACSL $NEXT_MAJOR.$NEXT_MINOR ($NEXT_CODENAME)"

  if ! grep -q -e " *$EA_CL_MSG_NEXT" $EA_CHANGELOG; then
      $SED -i "s/\($EA_CL_MSG_FUTURE\)/\1\n$FC_CL_LIN\n\n$FC_CL_LIN\n$EA_CL_MSG_NEXT/g" $EA_CHANGELOG
  fi

  WP_CHANGELOG="src/plugins/wp/Changelog"
  WP_CL_MSG_FUTURE="Plugin WP <next-release>"
  WP_CL_MSG_NEXT="Plugin WP $NEXT_MAJOR.$NEXT_MINOR ($NEXT_CODENAME)"

  if ! grep -q -e " *$WP_CL_MSG_NEXT" $WP_CHANGELOG; then
      $SED -i "s/\($WP_CL_MSG_FUTURE\)/\1\n$FC_CL_LIN\n\n$FC_CL_LIN\n$WP_CL_MSG_NEXT/g" $WP_CHANGELOG;
  fi

  # API doc
  find src -name '*.ml*' -exec $SED -i -e "s/Frama-C\(+\|~\)dev/${NEXT_MAJOR}.${NEXT_MINOR}-${NEXT_CODENAME}/gI" '{}' ';'

  # Manuals changes
  $SED -i "s/\(^\\\\section\*{Frama-C\(+\|~\)dev}\)/%\1\n\n\\\\section\*{$NEXT_MAJOR.$NEXT_MINOR ($NEXT_CODENAME)}/g" \
    doc/userman/user-changes.tex
  $SED -i "s/\(^\\\\section\*{Frama-C\(+\|~\)dev}\)/%\1\n\n\\\\section\*{$NEXT_MAJOR.$NEXT_MINOR ($NEXT_CODENAME)}/g" \
    doc/developer/changes.tex
  $SED -i "s/\(^\\\\subsection{Frama-C\(+\|~\)dev}\)/%\1\n\n\\\\subsection{Frama-C $NEXT_CODENAME}/g" \
    doc/aorai/main.tex
  $SED -i "s/\(^\\\\section\*{Frama-C\(+\|~\)dev}\)/%\1\n\n\\\\section\*{Frama-C $NEXT_MAJOR.$NEXT_MINOR ($NEXT_CODENAME)}/g" \
    src/plugins/acsl-importer/doc/manual/user-changes.tex
  $SED -i "s/\(^\\\\section\*{E-ACSL \\\\eacslpluginversion \\\\eacslplugincodename}\)/%\1\n\n\\\\section\*{E-ACSL $NEXT_MAJOR.$NEXT_MINOR $NEXT_CODENAME}/g" \
    src/plugins/e-acsl/doc/userman/changes.tex
  $SED -i "s/\(^\\\\subsection\*{Version Frama-C\(+\|~\)dev}\)/%\1\n\n\\\\subsection\*{Version $NEXT_CODENAME-$NEXT_MAJOR}/g" \
    src/plugins/e-acsl/doc/refman/changes_modern.tex
  $SED -i "s/\(^\\\\section\*{Frama-C\(+\|~\)dev}\)/%\1\n\n\\\\section\*{Frama-C $NEXT_MAJOR.$NEXT_MINOR ($NEXT_CODENAME)}/g" \
    src/plugins/volatile/doc/manual/user-changes.tex

  # Reference configuration
  $SED -i "s/Frama-C [1-9][0-9]\.[0-9]/Frama-C $NEXT_MAJOR.$NEXT_MINOR/gI" \
    reference-configuration.md

fi
