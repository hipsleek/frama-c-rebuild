#! /usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# Note:
#
# This script *shall not* be run locally, it is meant to run in the
# Frama-C CI when it is in publish or release mode.
# The following variables are necessary:
# - CI (provided by the GitLab pipeline)
# - PUBLISH or RELEASE set to yes
# - REPOSITORY: repository to publish
# - BRANCH: the branch to push

##########################################################################

# Sanity checks

if [[ "$CI" != "true" ]]; then
  echo "Error: Do NOT run this script locally"
  exit 2
fi

if [[ "$PUBLISH" != "yes" && "$RELEASE" != "yes" ]]; then
  echo "Error: Publication only available in publish or release mode"
  exit 2
fi

if [[ "$REPOSITORY" == "" ]]; then
  echo "Error: REPOSITORY is not defined"
  exit 2
fi

if [[ "$BRANCH" == "" ]]; then
  echo "Error: BRANCH is not defined"
  exit 2
fi

if [[ "$PUBLISH" == "yes" && "$BRANCH" != "master" ]]; then
  echo "Error: In PUBLISH mode, only master can be pushed"
  exit 2
fi

if [[ "$RELEASE" == "yes" && ! "$BRANCH" =~ stable/* ]]; then
  echo "Error: In RELEASE mode, only a stable/ branch can be pushed"
  exit 2
fi

# Actual work

echo "$FRAMA_CI_BOT_SSH_PRIVATE" | base64 -d > nix/frama-c-public/id_ed25519
chmod 400 nix/frama-c-public/id_ed25519

if [[ "$REPOSITORY" != "frama-c" ]] ; then
  DIRECTORY="nix/frama-c-public/$REPOSITORY"
  git clone "git@git.frama-c.com:frama-c/$REPOSITORY.git" "$DIRECTORY"
else
  DIRECTORY="."
fi

git -C "$DIRECTORY" push "git@git.frama-c.com:pub/$REPOSITORY" "origin/$BRANCH":"refs/heads/$BRANCH"
