#!/usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

commit="$(git ls-remote https://git-token:$FRAMA_CI_BOT_API_TOKEN@git.frama-c.com/frama-c/wp-cache.git HEAD)"
if [ $? != 0 ]; then exit 1; fi

commit=$(echo "$commit" | cut -f1)

echo -e "\e[1;31mSelected cache commit: $commit\e[0m"

cat >./nix/wp-cache.nix << EOL
{ lib, stdenv } :
stdenv.mkDerivation rec {
  name = "frama-c-wp-cache";
  src = fetchGit {
           url = "git@git.frama-c.com:frama-c/wp-cache.git" ;
           rev = "$commit" ;
           shallow = true ;
         };
  installPhase = "touch \$out";
}
EOL
