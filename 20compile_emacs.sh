#!/bin/bash

# This script compiles Emacs from Git source.
# All pre-requisites were installed by 10install_linux.sh
# See http://www.emacswiki.org/emacs/EmacsSnapshotAndDebian

cd ~/repos
pwd

if [ -d emacs ]; then
    cd emacs
    git pull
    git clean -xfd
else
    #git clone --depth 1 git://git.sv.gnu.org/emacs.git
    git clone --depth 1 https://github.com/emacs-mirror/emacs
    cd emacs
fi

./autogen.sh
./configure
make -j4
sudo make install
