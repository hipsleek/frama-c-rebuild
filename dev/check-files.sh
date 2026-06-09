#!/bin/bash -eu
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

THIS_SCRIPT="$0"
LINT=check-lint
HDRCK=check-headers
DO_LINT="yes"
DO_HDRCK="yes"
REFERENCE=
MODE="all"

while [ $# -gt 0 ]
do
    case "$1" in
        "-h"|"-help"|"--help")
            echo "${THIS_SCRIPT} [OPTIONS]"
            echo "OPTIONS"
            echo "  -h|-help|--help    print this message and exit"
            echo "  -f|--fix           fix files"
            echo "  -c|--commit        check modified files to be committed only"
            echo "  -p|--push range    check modified files to be pushed only"
            echo "  --no-headers       do not check headers"
            echo "  --no-lint          do not check lint"
            exit 0
            ;;
        "-f"|"--fix")
            LINT=lint
            HDRCK=headers
            ;;
        "-p"|"--push")
            shift
            DIFF_ARG=$1
            MODE="push"
            ;;
        "-c"|"--commit")
            DIFF_ARG=--cached
            MODE="commit"
            ;;
        "--no-headers")
            DO_HDRCK="no"
            ;;
        "--no-lint")
            DO_LINT="no"
            ;;
        *)
            echo "Unknown option '$1'"
            exit 2
            ;;
    esac
    shift
done

if [ "$MODE" = "all" ]; then
  if [ $DO_LINT = "yes" ] ; then
    make $LINT
  fi
  if [ $DO_HDRCK = "yes" ] ; then
    # Don't define HDRCK_EXTRA, that is required by external plugins
    make $HDRCK
  fi
else
  TMP_STAGED=$(mktemp)
  TMP_UNSTAGED=$(mktemp)
  TMP_INTER=$(mktemp)
  TMP_INPUT=$(mktemp)

  cleanup () {
    rm -f "$TMP_STAGED" "$TMP_UNSTAGED" "$TMP_INPUT" "$TMP_INTER"
  }
  trap cleanup exit
  git diff -z --diff-filter ACMR --name-only $DIFF_ARG | sort -z > "$TMP_STAGED"
  git diff -z --diff-filter DMR --name-only | sort -z > "$TMP_UNSTAGED"

  if [ ! -s "$TMP_STAGED" ];
  then
    echo "No staged modification since last $MODE, nothing to do."
    exit 0
  fi

  if [ -s "$TMP_UNSTAGED" ];
  then
    comm -12 --zero-terminated "$TMP_STAGED" "$TMP_UNSTAGED" > "$TMP_INTER"
    if [ -s "$TMP_INTER" ];
    then
      echo "Cannot validate push."
      echo "The following staged files have been modified, renamed or deleted."
      while IFS= read -r -d $'\0' f; do
        echo "- $f"
      done < "$TMP_INTER"
      exit 1
    fi
  fi

  if [ $DO_LINT = "yes" -a -s "$TMP_STAGED" ] ; then
    git check-attr -za --stdin < "$TMP_STAGED" > "$TMP_INPUT"
    make $LINT LINTCK_FILES_INPUT="$TMP_INPUT"
  fi
  if [ $DO_HDRCK = "yes" -a -s "$TMP_STAGED" ] ; then
    git check-attr -z header_spec --stdin < "$TMP_STAGED" > "$TMP_INPUT"
    # Don't define HDRCK_EXTRA, that is required by external plugins
    make $HDRCK HDRCK_FILES_INPUT="$TMP_INPUT"
  fi
fi
