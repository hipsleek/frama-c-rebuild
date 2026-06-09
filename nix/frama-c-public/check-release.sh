#! /usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# This script is meant to be run in CI. In particular, it requires the
# following environment variables:
# - CI_COMMIT_REF_NAME (GitLab variable): the branch the commit belongs to
# - DEFAULT (CI variable): the default branch configured in .gitlab-ci.yml
# - PUBLISH (CI variable): indicating publish mode in .gitlab-ci.yml
#
# It checks that:
# - we are not in publish mode,
# - we are running the release pipeline on the default branch,
# - the default branch is a stable branch with a name coherent with the version
# - the tag of the commit is coherent with the version number
# - the version in the Opam file is coherent with the version number
# - the manual version in the Opam file is coherent with the version number

##########################################################################

function exit_red {
  echo -e "\e[31m$1\e[0m"
  exit 1
}
function echo_green {
  echo -e "\e[32m$1\e[0m"
}

VERSION="$(cat VERSION)"
VERSION_SAFE="$(cat VERSION | sed 's/~/-/')"
VERSION_OPAM=$(cat opam | grep "^version" | sed 's/version: \"\(.*\)\"/\1/')
TAG="$(git describe --tag)"
CODENAME="$(cat VERSION_CODENAME)"
LOWER_CODENAME="$(echo "$CODENAME" | tr '[:upper:]' '[:lower:]')"

if [[ "$PUBLISH" == "no" ]] ; then
  echo_green "We are not in publish mode"
else
  exit_red   "PUBLISH MODE DETECTED"
fi

if [[ "$DEFAULT" == "$CI_COMMIT_REF_NAME" ]] ; then
  echo_green "The branch is the default branch"
else
  exit_red   "THIS BRANCH ($CI_COMMIT_REF_NAME) IS NOT THE DEFAULT ($DEFAULT)"
fi

if [[ "$DEFAULT" == "stable/$LOWER_CODENAME" ]] ; then
  echo_green "The default branch is stable"
else
  exit_red   "$DEFAULT IS NOT A STABLE BRANCH"
fi

if [[ "$TAG" == "$VERSION_SAFE" ]] ; then
  echo_green "Git tag and version are consistent"
else
  exit_red   "GIT TAG $TAG IS NOT CONSISTENT WITH (SAFE) VERSION $VERSION_SAFE"
fi

if [[ "$VERSION" == "$VERSION_OPAM" ]] ; then
  echo_green "Opam version and version are consistent"
else
  exit_red   "VERSION $VERSION AND OPAM VERSION $VERSION_OPAM ARE NOT CONSISTENT"
fi
