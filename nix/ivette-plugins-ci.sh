#!/usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# DEFAULT variable can be configured to indicate reference branch when the
# current branch does not exist in a plugin.

set -euxo pipefail

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

git_current_branch="$(git branch --show-current)"
: "${git_current_branch:=${CI_COMMIT_REF_NAME:-}}"
echo "currently on branch $git_current_branch"

temporary="$(mktemp -d)"
callsite="$(pwd)"

cleanup () {
  cd "$callsite"
  if [[ -n $temporary ]];
  then rm -rf "$temporary"
  fi
  git worktree prune
}

trap cleanup EXIT

git worktree add "$temporary" "$(git rev-parse HEAD)"
cd "$temporary"

declare -A plugins=( )
declare -A ivette_plugins=( )

if [[ ! -f "./nix/ivette-plugins.txt" ]]; then
  echo "NO ./nix/ivette-plugins.txt FOUND!"
  exit 2
fi

while read -r var fcplugin ivetteplugin; do
  plugins[$var]=$fcplugin
  ivette_plugins[$var]=$ivetteplugin
done < "./nix/ivette-plugins.txt"

for plugin in ${!plugins[@]}; do
  location="${plugins[$plugin]}"
  if [ -n "$location" ] && [ "$location" != "none" ]; then
    repo="https://git-token:$FRAMA_CI_BOT_API_TOKEN@git.frama-c.com/$location"
    branch="$(get_matching_branch "$repo" "$git_current_branch")"
    git clone --depth=1 --branch="$branch" "$repo" "src/plugins/$plugin"
  fi
  location="${ivette_plugins[$plugin]}"
  if [ -n "$location" ] && [ "$location" != "none" ]; then
    repo="https://git-token:$FRAMA_CI_BOT_API_TOKEN@git.frama-c.com/$location"
    branch="$(get_matching_branch "$repo" "$git_current_branch")"
    git clone --depth=1 --branch="$branch" "$repo" "ivette/src/frama-c/plugins/$plugin"
  fi
done

# Build Frama-C API with the new plugins
dune build -j2 @install
make -C ivette api

# Build Ivette
make -C ivette check-lint
make -C ivette dist
