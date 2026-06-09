#!/bin/sh

ERGO=$(alt-ergo --version)
WHY3=$(why3 --version)

echo "----------------------------------------------------------"
echo "WP Requirements for Qualif Tests"
echo "----------------------------------------------------------"

#------ Alt-Ergo version
echo "1. The Alt-Ergo theorem prover, version ${ERGO}"

#------ Why3 version
echo "2. The ${WHY3}"

#------ FRAMAC_WP_CACHEDIR
if [ "${FRAMAC_WP_CACHEDIR}" = "" ]; then
    echo "3. Error: undefined FRAMAC_WP_CACHEDIR variable"
else
    echo "3. The environment variable FRAMAC_WP_CACHEDIR is defined"
fi
#------ FRAMAC_WP_CACHE
if [ "${FRAMAC_WP_CACHE}" = "" ]; then
    echo "4. Error: undefined FRAMAC_WP_CACHE variable"
else
    echo "4. The environment variable FRAMAC_WP_CACHE is defined"
fi
echo "----------------------------------------------------------"
