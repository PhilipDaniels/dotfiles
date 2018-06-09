#!/bin/bash

# This script installs the Microsoft Office 2007 fonts such as
# Consolas and Calibri.

# The 10 script does this already:
# sudo apt-get install font-manager cabextract


TMPDIR=fonttemp
DESTDIR=/usr/local/share/fonts
DOCACHE=0

# Install the Microsoft fonts.
if [ -f $DESTDIR/CONSOLA.TTF ] ; then
    echo "The Microsoft Office 2007 fonts (Consolas, Calibri etc.) are already in $DESTDIR"
else
    set -x

    rm -rf $TMPDIR
    mkdir $TMPDIR
    cd $TMPDIR
    wget http://download.microsoft.com/download/E/6/7/E675FFFC-2A6D-4AB0-B3EB-27C9F8C8F696/PowerPointViewer.exe
    cabextract -L -F ppviewer.cab PowerPointViewer.exe
    cabextract ppviewer.cab

    sudo cp *.TTF $DESTDIR
    sudo chmod -R 777 $DESTDIR

    cd ..
    rm -rf $TMPDIR
    set +x
    DOCACHE=1
fi


# Install the Chris Simpkins font collection.
if [ -f $DESTDIR/AverageMono.ttf ]; then
    echo "The Chris Simpkins Codeface fonts are already installed in $DESTDIR"
else
    set -x
    rm -rf $TMPDIR
    mkdir $TMPDIR
    cd $TMPDIR
    wget https://github.com/chrissimpkins/codeface/releases/download/font-collection/codeface-fonts.zip
    unzip codeface-fonts.zip
    sudo find . -type f -iname '*.ttf' -exec cp '{}' $DESTDIR \;
    sudo chmod -R 777 $DESTDIR
    cd ..
    rm -rf $TMPDIR
    set +x
    DOCACHE=1
fi


if [ "$DOCACHE" == "1" ]; then
    set -x
    fc-cache -fv
    sudo fc-cache -fv
    set +x
fi

rm -rf $TMPDIR
