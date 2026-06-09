#!/usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# DEFAULT variable can be configured to indicate reference branch when the
# current branch does not exist in a dependency or in Frama-C.
#
# OCAML must be set to the right version of OCAML (format: N_MM or N.MM)

set -euxo pipefail

if [[ $# != 1 ]];
then
  cat <<EOF
usage: OCAML=N_MM $0 <plugin>
  $0 <plugin_name> run CI on this plugin
EOF
  exit 2
fi

if [ -z ${OCAML+x} ]; then
  echo "OCAML variable must be set to a version of OCaml"
  exit 2
fi

# Normalize version for Nix
OCAML=${OCAML/./_}

OUTOPT="--no-out-link"
if [ ! -z ${OUT+x} ]; then
  OUTOPT="-o $(pwd)/$OUT"
fi

DEFAULT=${DEFAULT:-master}

# prints
# - "$2" if it is a branch name in remote $1,
# - else "$DEFAULT" if it is set and $DEFAULT is a branch name in remote $1,
# - else master
get_matching_branch () {
  if   git ls-remote --quiet --exit-code "$1" "$2" >/dev/null 2>/dev/null;
  then echo "$2"
  elif git ls-remote --quiet --exit-code "$1" "$DEFAULT" >/dev/null 2>/dev/null;
  then echo "$DEFAULT"
  else echo master
  fi
}

#         fc-dir     nix-dir
fc_dir="$(dirname "$(dirname "$(readlink -f "$0")")")"

git_current_branch="$(git branch --show-current)"
: "${git_current_branch:=${CI_COMMIT_REF_NAME:-}}"
echo "currently on branch $git_current_branch"
[[ -n $git_current_branch ]]
plugin_repo=

cleanup () {
  if [[ -n $plugin_repo ]];
  then rm -rf "$plugin_repo"
  fi
}

trap cleanup EXIT

OPTS="--arg frama-c-repo $fc_dir"

plugin=$1

cd "$(mktemp -d)"

# the hash of the derivation depends on the directory name
mkdir "$plugin"
cd "$plugin"
plugin_repo="$(readlink -f .)"
plugin_url="git@git.frama-c.com:frama-c/$plugin.git"

plugin_branch="$(get_matching_branch "$plugin_url" "$git_current_branch")"
echo "using branch $plugin_branch of $plugin_url"
git clone --depth=1 --branch="$plugin_branch" "$plugin_url" .

declare -A deps=( )
declare -A dirs=( )

if [[ -f "./nix/dependencies" ]]; then
  while read -r var value; do
    deps[$var]=$value
  done < "./nix/dependencies"

  for repo in ${!deps[@]}; do
    # the hash of the derivation depends on the directory name
    mkdir "../$repo"
    directory="$(readlink -f ../$repo)"
    dirs[$repo]=$directory

    url=${deps[$repo]}
    branch="$(get_matching_branch "$url" "$git_current_branch")"
    echo "using branch $branch of $repo at $directory"
    # clone
    git clone --depth=1 --branch="$branch" "$url" "$directory"

    OPTS="$OPTS --arg $repo $directory"
  done
fi

# run the build
nix-build $OUTOPT "./nix/pkgs.nix" $OPTS -A ocaml-ng.ocamlPackages_$OCAML."$plugin"

cd "$fc_dir"

for repo in ${!dirs[@]}; do
  if [[ -n ${dirs[$repo]} ]];
  then rm -rf "${dirs[$repo]}"
  fi
done

rm -rf "$plugin_repo"
