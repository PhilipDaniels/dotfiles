#!/bin/bash

# This script sets up TightVNC. It should be run once after
# tightvnc is installed.

sudo cp tightvncserver /etc/init.d
sudo chmod ugo+x /etc/init.d/tightvncserver
sudo update-rc.d tightvncserver defaults
sudo /etc/init.d/tightvncserver start
cp -f xstartup ~/.vnc/xstartup

