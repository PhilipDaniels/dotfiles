#!/bin/bash

# This script is to be run once only.

# Manual: ensure /etc/hosts has entries for IPs on my local net.
# sudo vim /etc/hosts
echo '192.168.1.200 hyperbox' | sudo tee -a /etc/hosts
echo '192.168.1.212 wikibox'  | sudo tee -a /etc/hosts


# Remove xterm and uxterm from the menu.
echo "NoDisplay=true" >> /usr/share/applications/debian-xterm.desktop
echo "NoDisplay=true" >> /usr/share/applications/debian-uxterm.desktop

#dpkg --add-architecture i386

