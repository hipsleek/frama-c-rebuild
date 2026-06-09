#! /usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

if [[ $# != 1 && $# != 2 ]]; then
  echo "Usage: $0 <plugin-name> [<directory>]"
  exit 2
fi
if [[ $# == 1 ]]; then
  directory="."
else
  directory=$2
fi

if [[ ! -d $directory ]]; then
  echo "'$directory': not such file or directory"
  exit 17
fi

echo "Target directory is '$directory'"

dune_file=$directory/dune

if [[ -f $dune_file ]]; then
  echo "'$dune_file' file already exists."
  exit 17
fi

cat > $dune_file <<EOF
( library
  (name $1)
  (public_name frama-c-$1.core)
  (flags -open Frama_c_kernel :standard)
  (libraries frama-c.kernel)
)

(plugin (optional) (name $1) (libraries frama-c-$1.core) (site (frama-c plugins)))
EOF
