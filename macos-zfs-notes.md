---

# MacOS worker with OpenZFS

Get a worker running with `rsync` make the following changes to the base images

```shell=zfs
sudo -i
for image in busybox macos-homebrew-ocaml-4.14 macos-homebrew-ocaml-5.0 ; do
cd /Users/$image
mkdir home
mv ./*(D) home
mv home/local .
cd local
ln -s /usr/local
done
```

The `ln -s /usr/local` is required as the spec contains this line which references `~` rather than `/usr/local/` or `/opt/homebrew` which makes it architecture independant.

```
 (run (shell "ln -f ~/local/bin/opam-2.1 ~/local/bin/opam"))
```

Remove macFuse to avoid any possibility of a conflict.

Install OpenZFS and accept the kernel extension via system preferences.

```
brew install openzfs
```

copy zfs to ~/zfs, patch the executables to remove references to `/usr/local` and add the executables to $PATH:

```
cp -r /usr/local/zfs ~/zfs
for dir in ~/zfs/lib/*.dylib ~/zfs/bin/* ; do
for lib in `otool -L $dir | grep usr/local | cut -f 1 -d' ' | cut -f 2` ; do
new=`echo $lib | sed "s/usr\/local/Users\/vagrant/g"` ;
install_name_tool -change $lib $new $dir ;
done ;
done
export PATH=$PATH:~/zfs/bin
```

Create a ZFS pool

```
sudo mkfile 10G /Volumes/zfs
sudo zpool create pool1 /Volumes/zfs
sudo zfs set atime=off pool1
```

Turn off desktop icons as each ZFS volume becomes an icon!

```
defaults write com.apple.finder CreateDesktop -bool false
killall Finder
```

> Finder still uses lots of CPU so it's better for the worker to be signed out rather than sitting at the desktop.  The workers are at the desktop as they need Docker to be running.  The requirement for the Docker installation can be entry mitigated by creating `/usr/local/bin/docker` containing just `#!/bin/bash`.  This passes the Docker health check code.  Now, with a signed out worker, Finder isn't running at all.

Get my obuilder branch

```
cd ~/ocluster/obuilder
git remote add mtelvers https://github.com/mtelvers/obuilder
git fetch mtelvers
git checkout zfs
```

Run the worker

```
cd ~/ocluster
dune build
sudo -E ./_build/install/default/bin/ocluster-worker --connect /Users/vagrant/pool-macos-x86_64.cap --uid=1000 --brew-path=/usr/local --store=zfs:/Volumes/pool1 --state-dir=/var/lib/ocluster-worker --name=droy1 --capacity=1 --obuilder-prune-threshold=10 --verbosity=info
```


