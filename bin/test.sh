#!/bin/bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

THIS_SCRIPT="$0"
CONFIG="<default>"
VERBOSE=
CLEAN=
PREPARE=
USEWPCACHE=
UPDATE=
GENERATE=
LOGS=
TESTS=()
SAVE=
COVER=
HTML=
XML=
JSON=

PTESTS_DIR=()
DUNE_ALIAS=()
DUNE_OPT=()
DUNE_OPT_POST=()
DUNE_LOG=./.test-errors.log
ALIAS_NAME=ptests
LOCAL_WP_CACHE=$(pwd -P)/.wp-cache
FRAMAC_WP_CACHE_GIT=git@git.frama-c.com:frama-c/wp-cache.git

TEST_DIRS="tests src/plugins/*"
KERNEL_TEST_ALIASES="@src/runtest-frama_c_kernel @run-kernel-tests"

# --------------------------------------------------------------------------
# ---  Help Message
# --------------------------------------------------------------------------

function Usage
{
    echo "USAGE"
    echo ""
    echo "${THIS_SCRIPT} [OPTIONS|TESTS]..."
    echo ""
    echo "TESTS SPECIFICATION"
    echo ""
    echo "  Tip: use shell completion"
    echo ""
    echo "  <FILE>    single test file <FILE>"
    echo "  <DIR>     all tests in <DIR>,"
    echo "            or in directory tests/<DIR> (if it exists)"
    echo "            or in plugin src/plugins/<DIR> (if it exists)"
    echo "  kernel    all kernel tests, i.e. all tests in tests/, inline tests"
    echo "            and any test declared in kernel sources."
    echo ""
    echo "  -a|--all            run all config"
    echo "  -d|--default        run tests from default config only (by default)"
    echo "  -c|--config <name>  run tests from specified config only"
    echo ""
    echo ""
    echo "OPTIONS"
    echo ""
    echo "  -n|--name <alias>   set dune alias name (default to ptests)"
    echo "  -r|--clean          clean (remove all) test results (includes -p)"
    echo "  -p|--ptests         prepare (all) dune files"
    echo "  -w|--wp-cache       use (clone/pull/update) WP-cache"
    echo "  -l|--logs           print output of tests (single file, no diff)"
    echo "  -u|--update         update oracles (and WP-cache)"
    echo "  -g|--generate       Generate new oracles and update oracles"
    echo "  -s|--save           save dune logs into $DUNE_LOG"
    echo "  -v|--verbose        print executed commands"
    echo "  --coverage          compute test coverage in html format"
    echo "  --coverage-xml      compute test coverage in Cobertura XML format"
    echo "  --coverage-json     compute test coverage in Coveralls JSON format"
    echo "  -h|--help           print this help"
    echo ""
    echo "TRAILING OPTIONS"
    echo ""
    echo "  All arguments passed after a double dash '--' are passed to dune"
    echo "  For example in 'test.sh -r -u tests -- -j 12', '-j 12' will be"
    echo "  passed as a dune argument"
    echo ""
    echo "VARIABLES"
    echo ""
    echo "  FRAMAC_WP_CACHE"
    echo "    Management mode of wp-cache (default is offline or update when -u)"
    echo ""
    echo "  FRAMAC_WP_QUALIF"
    echo "  FRAMAC_WP_CACHEDIR"
    echo "    Absolute path to wp-cache directory (git clone locally by default)"
    echo ""
    echo "  FRAMAC_DEVONLY_OPTIONS_PRE"
    echo "  FRAMAC_DEVONLY_OPTIONS_POST"
    echo "    Options that frama-c will pre/ap-pend to its actual command-line"
    echo ""
}

# --------------------------------------------------------------------------
# ---  Utilities
# --------------------------------------------------------------------------

function Head()
{
    echo "# $*"
}

function Error ()
{
    echo "Error: $*"
    exit 1
}

function ErrorUsage ()
{
    echo "Error: $*"
    echo "USAGE: ${THIS_SCRIPT} -h"
    exit 1
}

function Echo()
{
    [ "$VERBOSE" != "yes" ] || echo "$@"
}

function Run
{
    Echo "> $*"
    "$@"
}

function Cmd
{
    Run "$@" || Error "(command exits $?): $*"
}

function RequiredTools
{
    local tool
    for tool in "$@" ; do
        which "$tool" >/dev/null 2>&1 || Error "Executable not found: $tool"
    done
}

# --------------------------------------------------------------------------
# ---  Command Line Processing
# --------------------------------------------------------------------------

while [ "$1" != "" ]
do
    case "$1" in
        "-h"|"-help"|"--help")
            Usage
            exit 0
            ;;
        "-a"|"--all")
            CONFIG="<all>"
            ;;
        "-d"|"--default")
            CONFIG="<default>"
            ;;
        "-c"|"--config")
            CONFIG=$2
            shift
            ;;
        "-r"|"--clean")
            CLEAN=yes
            PREPARE=yes
            ;;
        "-p"|"--ptests")
            PREPARE=yes
            ;;
        "-w"|"--wp-cache")
            USEWPCACHE=yes
            ;;
        "-u"|"--update")
            UPDATE=yes
            ;;
        "-g"|"--generate")
            GENERATE=yes
            ;;
        "-v"|"--verbose")
            VERBOSE=yes
            ;;
        "-l"|"--logs")
            LOGS=yes
            ;;
        "-s"|"--save" )
            SAVE=yes
            ;;
        "--coverage")
            COVER=yes
            HTML=yes
            ;;
        "--coverage-xml")
            COVER=yes
            XML=yes
            ;;
        "--coverage-json")
            COVER=yes
            JSON=yes
            ;;
        "-n"|"--name")
            ALIAS_NAME=$2
            shift
            ;;
        "--")
            shift
            break
            ;;
        *)
            if [[ "$1" =~ ^@ ]]; then
                Head "Register test on alias $1"
                DUNE_ALIAS+=("$1")
            elif [ "$1" != "${1#/}" ]; then
                Error "Dune only accepts relative path, $1 is absolute"
            elif [ -f "$1" ] || [ -d "$1" ]; then
                TESTS+=("$1")
            elif [ -d "tests/$1" ]; then
                TESTS+=("tests/$1")
            elif [ -d "src/plugins/$1" ]; then
                TESTS+=("src/plugins/$1")
            elif [ "$1" == "kernel" ]; then
                Head "Register kernel tests"
                TESTS+=("tests/")
                DUNE_ALIAS+=("$KERNEL_TEST_ALIASES")
            else
                ErrorUsage "'$1' is neither a file/directory or a dune alias"
            fi
            ;;
    esac
    shift
done

if [ "$UPDATE" = "yes" ] || [ "$GENERATE" = "yes" ]; then
    DUNE_OPT+=("--auto-promote")
fi

if [ "$VERBOSE" = "yes" ]; then
  DUNE_OPT+=("--display=short")
  DUNE_OPT+=("--always-show-command-line")
fi

if [ "$COVER" = "yes" ]; then
    DUNE_OPT+=("--workspace=dev/dune-workspace.cover")
fi

# Pass all the remaining options (after '--') to dune at the end of the command
DUNE_OPT_POST=("$@")

# --------------------------------------------------------------------------
# ---  WP Cache Environment
# --------------------------------------------------------------------------

function SetWPCache
{
    if [ "$FRAMAC_WP_CACHE" = "" ]; then
        Head "FRAMAC_WP_CACHE=$1"
        export FRAMAC_WP_CACHE="$1"
    elif [ "$FRAMAC_WP_CACHE" = "$1" ]; then
        Head "FRAMAC_WP_CACHE=$FRAMAC_WP_CACHE"
    else
        Head "FRAMAC_WP_CACHE=$FRAMAC_WP_CACHE (overrides $1)"
    fi
}

function SetEnv
{
    if [ "$USEWPCACHE" = "yes" ] && [ "$UPDATE" = "yes" ]; then
        SetWPCache "update"
    else
        SetWPCache "offline"
    fi

    if [ "$FRAMAC_WP_QUALIF" != "" ]; then
        export FRAMAC_WP_CACHEDIR="$FRAMAC_WP_QUALIF"
    else
        export FRAMAC_WP_CACHEDIR="$LOCAL_WP_CACHE"
    fi
    Echo "# FRAMAC_WP_CACHEDIR=$FRAMAC_WP_CACHEDIR"

    if [ -f "$FRAMAC_WP_CACHEDIR" ]; then
        Error "$FRAMAC_WP_CACHEDIR is not a directory"
    fi

    if ! [ "$1" = "${1#/}" ]; then
        Error "Requires an absolute path to $FRAMAC_WP_CACHEDIR"
    fi
}

function GetCache
{
    if [ ! -d "$FRAMAC_WP_CACHEDIR" ]; then
        Head "Cloning WP cache (from $FRAMAC_WP_CACHE_GIT to $FRAMAC_WP_CACHEDIR)..."
        RequiredTools git
        Cmd git clone "$FRAMAC_WP_CACHE_GIT" "$FRAMAC_WP_CACHEDIR"
    else
        Head "Pull WP cache (to $FRAMAC_WP_CACHEDIR)..."
        RequiredTools git
        Run git -C "$FRAMAC_WP_CACHEDIR" pull --rebase
    fi
}

function PrepareWPCache
{
    SetEnv
    if [ "$USEWPCACHE" = "yes" ]; then
        GetCache
    fi
}

# --------------------------------------------------------------------------
# ---  Coverage
# --------------------------------------------------------------------------

function PrepareCoverage
{
    BISECT_FILE="$(pwd -P)/_bisect/bisect-"
    export BISECT_FILE
    if [ "$COVER" = "yes" ] ;
    then
        Cmd rm -rf _coverage
        Cmd rm -rf _bisect
        Cmd mkdir _coverage
        Cmd mkdir _bisect
    fi
}

function GenerateCoverage
{
    if [ "$COVER" = "yes" ] ;
    then
        Head "Generating coverage in _coverage ..."
        if [ "$HTML" = "yes" ] ;
        then
            Cmd bisect-ppx-report html --coverage-path=_bisect --tree
        fi
        if [ "$XML" = "yes" ] ;
        then
            Cmd bisect-ppx-report cobertura --coverage-path=_bisect _coverage/coverage_report.xml
        fi
        if [ "$JSON" = "yes" ] ;
        then
            Cmd bisect-ppx-report coveralls --coverage-path=_bisect _coverage/coverage_report.json
        fi
    fi
}

# --------------------------------------------------------------------------
# ---  Test Suite Preparation
# --------------------------------------------------------------------------

function GenerateDuneFiles
{
    Head "Generating dune files..."
    Cmd make run-ptests
}

function CheckDuneFiles
{
    local default_file="tests/syntax/result/dune"
    if [ "$PREPARE" != "yes" ] ;
    then
        if [ ! -f "$default_file" ] ;
        then
            GenerateDuneFiles
        else
            DATE_TEST_MODIFICATION=$(find -L "${TESTS[@]}" -type f \
                                    -not -path "*/result*/*" \
                                    -not -path "*/oracle*/*" \
                                    -exec stat --printf "%Y\n" {} \+ | \
                                    sort -n -r | head -n 1)
            DATE_TEST_GENERATION=$(stat $default_file --printf "%Y\n")
            if [ "$DATE_TEST_MODIFICATION" -gt "$DATE_TEST_GENERATION" ] ;
            then
                GenerateDuneFiles
            fi
        fi
    fi
}

function PrepareTests
{
    local dir
    if [ "${#TESTS[@]}" == 0 ] && (( "${#DUNE_ALIAS[@]}" == 0 )); then
        DUNE_ALIAS+=("@runtest")
        for dir in $TEST_DIRS ; do
            if [ -d "$dir" ]; then
                TESTS+=("$dir")
            fi
        done
    fi

    if [ "$CLEAN" = "yes" ]
    then
        Head "Cleaning all tests..."
        Cmd make clean-tests
    fi
    if [ "$PREPARE" = "yes" ]
    then
        GenerateDuneFiles
    fi
}

# --------------------------------------------------------------------------
# ---  Test Dir Alias
# --------------------------------------------------------------------------

[ "$DUNE_LOG" = "" ] || rm -rf "$DUNE_LOG"
function RunAlias
{
    Head "Running tests..."
    # Do not use "" here to avoid 'unknown option' on options with arguments.
    # shellcheck disable=SC2206
    local commands=(${DUNE_OPT[@]} $@ ${DUNE_OPT_POST[@]})

    if [ "$DUNE_LOG" = "" ]; then
        Run dune build "${commands[@]}"
    elif [ "$SAVE" != "yes" ] && [ "$VERBOSE" != "yes" ]; then
        Run dune build "${commands[@]}"
    else
        # note: the Run function cannot performs redirection
        echo "> dune build ${commands[*]} 2> >(tee -a $DUNE_LOG >&2)"
        dune build "${commands[@]}" 2> >(tee -a "$DUNE_LOG" >&2)
    fi
}

# --------------------------------------------------------------------------
# ---  Test Dir Processing
# --------------------------------------------------------------------------

function TestDir
{
    local alias cfg oracle
    case "$CONFIG" in
        "<all>")
            alias=$1/${ALIAS_NAME}
            cfg="(all configs)"
            oracle="*/oracle*"
            ;;
        "<default>")
            alias=$1/${ALIAS_NAME}_config
            cfg="(default config)"
            oracle="*/oracle"
            ;;
        *)
            alias=$1/${ALIAS_NAME}_config_$CONFIG
            cfg="(config $CONFIG)"
            oracle="*/oracle_$CONFIG"
            ;;
    esac

    FindPtestDir "$1"

    if [ -n "$(find -L "$1" -type d -path "$oracle")" ]; then
        Head "Register test on directory $1 $cfg"
        DUNE_ALIAS+=("@$alias")
    else
        Head "Register test on directory $1 (no ptests config)"
        # Non-ptests tests are registered below
    fi

    # Add the runtest target for the given test directory to add all non-ptests
    # tests to the run (cram tests, inline tests, etc.)
    if [[ ! "${DUNE_ALIAS[*]}" =~ "@runtest" ]]; then
        DUNE_ALIAS+=("@$1/runtest")
    fi
}

# --------------------------------------------------------------------------
# ---  Test File Processing
# --------------------------------------------------------------------------

function TestFile
{
    local dir file alias result cfg res
    dir=$(dirname "$1")
    file=$(basename "$1")

    case "$CONFIG" in
        "<all>")
            result="result*/"
            cfg="(all config)"
            ;;
        "<default>")
            result="result/"
            cfg="(default config)"
            ;;
        *)
            result="result_$CONFIG/"
            cfg="(config $CONFIG)"
            ;;
    esac

    for res in "$dir"/$result ; do
        # Ignore cases where no result folder is found
        [ -d "$res" ] || break

        if [ "$LOGS" = "yes" ]; then
            alias+=("@${res}${file}")
        else
            alias+=("@${res}${file%.*}.diff")
        fi
    done

    FindPtestDir "$dir"

    Head "Register test on file $1 $cfg"
    DUNE_ALIAS+=("${alias[@]}")
}

# --------------------------------------------------------------------------
# ---  Tests Processing
# --------------------------------------------------------------------------

function FindPtestDir
{
    local dir

    # If there is a tests/ subdirectory then start there
    if [ -d "$1/tests" ]; then
        dir="$1/tests"
    else
        dir="$1"
    fi

    if [ "$GENERATE" = "yes" ]; then
        # Look for the root folder of ptests, which contains ptests_config file
        # Only relative paths are accepted by dune, so the root folder of
        # every paths is '.'
        while [ -d "$dir" ] && [ "$dir" != "." ]; do
            if [ -f "$dir/ptests_config" ]; then
                PTESTS_DIR+=("$dir")
                break
            else
                dir=$(dirname "$dir")
            fi
        done
    fi
}

function Register
{
    local extension dir file
    while [ "$1" != "" ]
    do
        extension="${1##*.}"
        if [ "${extension}" == "t" ]; then
            Head "Register cramtest on file $1"
            DUNE_ALIAS+=("@${1%.*}")
        elif [ "${extension}" == "ml" ]; then
            dir=$(dirname "$1")
            file=$(basename "$1")
            Head "Register dune test on file $1"
            DUNE_ALIAS+=("@${dir}/runtest-${file%.*}")
        elif [ -d "$1" ]; then
            TestDir "$1"
        elif [ -f "$1" ]; then
            TestFile "$1"
        else
            Error "$1 is neither a file or a directory"
        fi
        shift
    done

    if [ "$GENERATE" = "yes" ]; then
        # Keep only one occurrence of each folder
        PTESTS_DIR=($(IFS=$'\n'; sort -u <<< "${PTESTS_DIR[*]}"))
    fi
}

# --------------------------------------------------------------------------
# ---  Tests Create New Oracles
# --------------------------------------------------------------------------

function MissingOracles
{
    if Run which frama-c-ptests >/dev/null 2>&1 ; then
        Cmd frama-c-ptests "$1" "${PTESTS_DIR[@]}" >/dev/null 2>&1
    else
        Cmd dune exec -- frama-c-ptests "$1" "${PTESTS_DIR[@]}" >/dev/null 2>&1
    fi
}

function CreateMissingOracles
{
    if [ "$GENERATE" = "yes" ]; then
        Head "Create missing oracles"
        MissingOracles "-create-missing-oracles"
    fi
}

function RemoveMissingOracles
{
    if [ "$GENERATE" = "yes" ]; then
        Head "Remove missing oracles"
        MissingOracles "-remove-empty-oracles"
    fi
}

# --------------------------------------------------------------------------
# ---  Tests Numbering
# --------------------------------------------------------------------------

function Status
{
    local nb dir
    #-- Count number of executed tests
    if [ "$1" != "" ] && [ -f "$1" ]; then
        if [ "$VERBOSE" = "yes" ] ; then
            #-- Total
            nb=$(grep -c "^frama-c-wtests " "$1")
            Head "Number of executed frama-c-wtests= $nb"
            #-- Details
            Head "Details by directory:"
            if  [ "$nb" != "0" ]; then
                for dir in "${TESTS[@]}" ; do
                    if [ -d "$dir" ]; then
                        nb=$(grep -c "^frama-c-wtests $dir" "$1")
                        [ "$nb" = "0" ] || echo "- $dir= $nb"
                    fi
                done
            fi
        fi
        if [ "$SAVE" != "yes" ]; then
            Cmd rm -f "$1"
        fi
    fi

    #-- Check wp-cache status
    if [ "$USEWPCACHE" = "yes" ] && [ "$UPDATE" = "yes" ]; then
        Head "Update $FRAMAC_WP_CACHEDIR and check status"
        RequiredTools git
        Run git -C "$FRAMAC_WP_CACHEDIR" add -A
        Run git -C "$FRAMAC_WP_CACHEDIR" status -s
    fi
}

# --------------------------------------------------------------------------
# ---  Main Program
# --------------------------------------------------------------------------

# Preparations
PrepareWPCache
PrepareCoverage
PrepareTests
CheckDuneFiles
Register "${TESTS[@]}"
CreateMissingOracles

# Running the tests
RunAlias $(IFS=$'\n'; sort -u <<<"${DUNE_ALIAS[*]}")
TESTS_STATUS=$?

# Post-treatments
RemoveMissingOracles
Status "$DUNE_LOG"
GenerateCoverage

# Exit with dune exit status
exit $TESTS_STATUS

# --------------------------------------------------------------------------
