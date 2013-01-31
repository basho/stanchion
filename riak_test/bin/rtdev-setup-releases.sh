#!/bin/bash

# Creates a mixed-version directory structure for running riak_test
# using rtdev-mixed.config settings. Should be run inside a directory
# that contains devrels for prior Riak CS releases. Easy way to create this
# is to use the rtdev-build-releases.sh script

: ${RTSTANCHION_DEST_DIR:="$HOME/rt/stanchion"}

rm -rf $RTSTANCHION_DEST_DIR
mkdir $RTSTANCHION_DEST_DIR
for rel in */dev; do
    vsn=$(dirname "$rel")
    mkdir "$RTSTANCHION_DEST_DIR/$vsn"
    cp -a "$rel" "$RTSTANCHION_DEST_DIR/$vsn"
done
cd $RTSTANCHION_DEST_DIR
git init
git add .
git commit -a -m "riak_test init"

