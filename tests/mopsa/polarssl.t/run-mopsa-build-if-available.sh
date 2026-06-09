#! /usr/bin/env bash

# Produces mopsa.db, either by running mopsa-build (if available), or by
# copying a precomputed mopsa-db otherwise.

if command -v mopsa-build 2>&1 >/dev/null; then
    mopsa-build make -j 2>make.err >make.log
    if [ $? != 0 ]; then
        echo "error running 'mopsa-build make':"
        cat make.log
        cat make.err
        exit 1
    fi
    mopsa-db -json > mopsa-db.json
else
    cp precomputed-mopsa-db.json mopsa-db.json
fi
