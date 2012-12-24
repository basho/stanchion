cwd=$(pwd)
cd /tmp/rtstanchion
git reset HEAD --hard
git clean -fd
rm -rf /tmp/rtstanchion/current
mkdir /tmp/rtstanchion/current
cd $cwd
cp -a dev /tmp/rtstanchion/current
cd /tmp/rtstanchion
git add .
git commit -a -m "riak_test init" --amend
