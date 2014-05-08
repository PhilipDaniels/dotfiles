#!/bin/bash

# This script installs apps. It can be run multiple times.

sudo apt-get update

# Non-X stuff.
sudo apt-get install samba winbind smbclient cifs-utils vim \
automake bc bison build-essential checkinstall libtool rsync \
flex exuberant-ctags g++ gcc gdb gettext git git-core htop make \
strace sudo usbutils links2

# Fonts. Doesn't require X.
sudo apt-get install ttf-bitstream-vera ttf-dejavu fonts-droid ttf-liberation \
fonts-inconsolata fonts-jura \
fonts-linuxlibertine fonts-quattrocento ttf-adf-gillius ttf-adf-ikarius \
ttf-adf-romande ttf-adf-switzera ttf-adf-verana ttf-radisnoir

# X core.
sudo apt-get install xfce4  # Lots of stuff!!!


# Extra X apps. 
# eterm = 2MB, terminator = 80MB, gnome-terminal = 120MB!
sudo apt-get install vim-gtk eterm terminator gimp gthumb \
gnome-specimen gksu
#chromium-browser tightvncserver





#sudo apt-get -y install wine-bin:i386

