#!/bin/bash

# This script is to be run once only.

# Manual: ensure /etc/hosts has entries for IPs on my local net.
# sudo vim /etc/hosts
# Enter line for hyperbox

# Remove xterm and uxterm from the menu.
echo "NoDisplay=true" >> /usr/share/applications/debian-xterm.desktop
echo "NoDisplay=true" >> /usr/share/applications/debian-uxterm.desktop

#dpkg --add-architecture i386

