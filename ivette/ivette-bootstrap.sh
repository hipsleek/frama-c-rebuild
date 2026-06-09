#! /usr/bin/env bash
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# --------------------------------------------------------------------------
# ---  Ivette bootstrapper for OPAM installation
# --------------------------------------------------------------------------

echo "Building Ivette"
USERCWD=`pwd`

# --------------------------------------------------------------------------

function InstallHelp()
{
    echo "Ivette Requirements:"
    echo "  - node 24 or later"
    echo "  - yarn (any version)"
    echo "Recommended Installation:"
    echo "  - install nvm (https://github.com/nvm-sh/nvm)"
    echo "  - run 'nvm install 24'"
    echo "  - run 'nvm use 24'"
    echo "  - run 'npm install --global yarn'"
    echo "  - run 'frama-c-gui'"
}

# --------------------------------------------------------------------------
echo "[1/3] Configuring"
# --------------------------------------------------------------------------

NODEJS=`node --version`
case $NODEJS in
    v22.*|v23.*|v24.*|v25.*)
        echo " - node $NODEJS found"
        ;;
    *)
        echo "The GUI requires node version 24 or later to be installed."
        echo
        InstallHelp
        exit 1 ;;
esac

YARNJS=`yarn --version`
case $YARNJS in
    1.*)
        echo " - yarn $YARNJS found"
        ;;
    *)
        echo "The GUI requires yarn to be installed."
        echo
        InstallHelp
        exit 1
        ;;
esac

SELF=`dirname $0`
cd $SELF/..
PREFIX=`pwd`

if [ -f $PREFIX/lib/frama-c/ivette.tgz ]
then
    echo " - prefix $PREFIX"
else
    echo "GUI archive not found ($PREFIX)"
    exit 1
fi

# --------------------------------------------------------------------------
echo "[2/3] Compiling GUI"
# --------------------------------------------------------------------------

IVETTE_TMP_DIR=`mktemp -d`
cd $IVETTE_TMP_DIR
tar zxf $PREFIX/lib/frama-c/ivette.tgz
cd ivette
make dist
if [ "$?" != "0" ]
then
    echo "Compilation Failed"
    rm -fr $IVETTE_TMP_DIR
    exit 2
fi

# --------------------------------------------------------------------------
echo "[3/3] Finalizing Installation"
# --------------------------------------------------------------------------

make PREFIX=$PREFIX install
if [ "$?" != "0" ]
then
    echo "Installation Failed"
    rm -fr $IVETTE_TMP_DIR
    exit 3
fi
ln -s $PREFIX/bin/frama-c-gui $PREFIX/bin/ivette
cd $USERCWD
rm -fr $IVETTE_TMP_DIR
rm -f $PREFIX/lib/frama-c/ivette.tgz

# --------------------------------------------------------------------------
echo "Launching GUI..."
# --------------------------------------------------------------------------
exec $PREFIX/bin/frama-c-gui $*

# --------------------------------------------------------------------------
