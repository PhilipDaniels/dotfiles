#!/bin/bash

# This script installs the Microsoft Office 2007 fonts such as
# Consolas and Calibri.

# The 10 script does this already:
# sudo apt-get install font-manager cabextract

set -e
set -x
mkdir msfonttemp
cd msfonttemp
wget http://download.microsoft.com/download/E/6/7/E675FFFC-2A6D-4AB0-B3EB-27C9F8C8F696/PowerPointViewer.exe
cabextract -L -F ppviewer.cab PowerPointViewer.exe
cabextract ppviewer.cab

sudo cp *.TTF /usr/local/share/fonts
sudo chmod -R 777 /usr/local/share/fonts

cd ..
rm -rf msfonttemp
