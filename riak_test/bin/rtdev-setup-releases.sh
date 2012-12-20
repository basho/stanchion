#!/bin/bash

# Creates a mixed-version directory structure for running riak_test
# using rtdev-mixed.config settings. Should be run inside a directory
# that contains devrels for prior Riak CS releases. Easy way to create this
# is to use the rtdev-build-releases.sh script

rm -rf /tmp/rtstanchion
mkdir /tmp/rtstanchion
for rel in */dev; do
    vsn=$(dirname "$rel")
    mkdir "/tmp/rtstanchion/$vsn"
    cp -p -P -R "$rel" "/tmp/rtstanchion/$vsn"
done
cd /tmp/rtstanchion
git init
git add .
git commit -a -m "riak_test init"
