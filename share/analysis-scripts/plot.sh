##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

target=$1

{
  echo -e "#slevel\talarms\ttime"

  for f in $target.*.eva
  do
    slevel=`sed -n 's/^[^.]\+.\([0-9]\+\).eva$/\1/p' <<<$f`

    if [ -n "$slevel" ]
    then
      echo -n -e "$slevel\t"
      tail --lines 1 $f.stats | cut --fields 1,2
    fi
  done
} > $target.dat
