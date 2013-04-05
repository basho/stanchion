#!/bin/bash

# bail out if things go south
set -e

: ${RTSTANCHION_DEST_DIR:="$HOME/rt/stanchion"}

cwd=$(pwd)
cd $RTSTANCHION_DEST_DIR
git reset HEAD --hard
git clean -fd
rm -rf $RTSTANCHION_DEST_DIR/current
mkdir $RTSTANCHION_DEST_DIR/current
cd $cwd
cp -a dev $RTSTANCHION_DEST_DIR/current
cd $RTSTANCHION_DEST_DIR
git add .
git commit -a -m "riak_test init" --amend

