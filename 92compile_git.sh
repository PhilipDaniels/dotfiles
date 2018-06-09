#!/bin/bash

# This script compiles Git from source.
# All pre-requisites were installed by 10install_programs.sh
# See https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

cd ~/repos
pwd

if [ -d git ]; then
    cd git
    git pull
    git clean -xfd
else
    git clone --depth 1 git://git.kernel.org/pub/scm/git/git.git
    cd git
fi

make configure
./configure
make all doc info
sudo make install install-doc install-html install-info
