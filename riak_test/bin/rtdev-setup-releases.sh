#!/bin/bash

# Creates a mixed-version directory structure for running riak_test
# using rtdev-mixed.config settings. Should be run inside a directory
# that contains devrels for prior Riak CS releases. Easy way to create this
# is to use the rtdev-build-releases.sh script

: ${STANCHION_DEST_DIR:="$HOME/rt/stanchion"}
mkdir -p $STANCHION_DEST_DIR

echo "Setting up releases from $(pwd):"
echo " - Creating $STANCHION_DEST_DIR"

rm -rf $STANCHION_DEST_DIR
mkdir -p $STANCHION_DEST_DIR
for rel in */dev; do
    vsn=$(dirname "$rel")
    echo " - Initializing $STANCHION_DEST_DIR/$vsn"
    mkdir "$STANCHION_DEST_DIR/$vsn"
    cp -p -P -R "$rel" "$STANCHION_DEST_DIR/$vsn"
done
cd $STANCHION_DEST_DIR
echo " - Creating rt stanchion git repository"
git init > /dev/null 2>&1

## Some versions of git and/or OS require these fields
git config user.name "Riak Test"
git config user.email "dev@basho.com"

git add .
git commit -a -m "riak_test init" > /dev/null 2>&1

