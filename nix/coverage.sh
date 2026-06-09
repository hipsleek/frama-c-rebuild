#!/usr/bin/env sh
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

if [ -z ${BISECT_DIR+x} ]; then
  echo "BISECT_DIR variable must indicate the reports directory"
  exit 2
fi

for i in _bisect/*.tar.xz ; do
  tar xfJ "$i" ;
done

combinetura ./*.xml -o report.xml --summary coverage-summary.txt

LINE=$(sed -n '2p' report.xml)
RATE=$(echo "$LINE" | sed -e 's/.*line-rate=\"\(.*\)\".*/\1/')

PERCENT="0.0"
if [ "$RATE" != "-nan" ]; then
  # Keep the "/1", bc DOES NOT use scale for anything else than division ...
  PERCENT=$(echo "scale=4; (100 * $RATE)/1" | bc -l)
fi

echo "Coverage: $PERCENT%"
