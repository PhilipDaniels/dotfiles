#!/bin/bash

# This script compiles and installs GNU Global from source.
# All pre-requisites were installed by 10install_programs.sh (it
# needs gperf, libtool and libtool-bin).
# See https://www.gnu.org/software/global/download.html

set -x
SRCDIR=gnuglobal
VER=global-6.5.2

cd ~/repos
pwd

if [ -d $SRCDIR ]; then
    rm -rf $SRCDIR
fi

mkdir $SRCDIR
cd $SRCDIR
wget http://tamacom.com/global/$VER.tar.gz
tar zxvf $VER.tar.gz
cd $VER

sh reconf.sh
./configure
make
sudo make install
cd ..
rm -rf $SRCDIR
