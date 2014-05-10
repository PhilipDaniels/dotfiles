#!/bin/bash

# This script installs apps. It can be run multiple times.
# To see the contents of a package do 'apt-file list package_name'

sudo apt-get update

# Non-X stuff. The fonts don't require X.
sudo apt-get install samba winbind smbclient cifs-utils vim \
automake bc bison build-essential checkinstall libtool rsync \
flex exuberant-ctags g++ gcc gdb gettext git git-core htop make \
Strace sudo usbutils links2 git ssh apt-file curl vim \
ttf-bitstream-vera ttf-dejavu fonts-droid ttf-liberation \
fonts-inconsolata fonts-jura fonts-linuxlibertine fonts-quattrocento \
ttf-adf-gillius ttf-adf-ikarius ttf-adf-romande ttf-adf-switzera \
ttf-adf-verana ttf-radisnoir

# X core.
#   Window Manager
#     [X] Openbox is lightest.
#     [ ] XFCE and LXDE are pretty good.
sudo apt-get install xorg openbox
#sudo apt-get install xfce4



# X apps. 
# File Manager
#   [X] xfe is nice! and works well over SSH.
#   [ ] PCManFM is fast
#   [ ] Thunar is OK.
# Text Editor
#   [X] vim-gtk
#   [X] geany, small about 10Mb. Might as well have.
# Terminal
#   [X] Terminator - has solarized theme built-in!
#   [ ] ETerm - very small but a bit "off". 
#   [ ] LXTerminal - OK, small, about 3Mb. Has tabs.
#       No real need if you are installing terminator.
# Web browser
#   [X] iceweasel (this is Firefox). About 60Mb.
#   [X] chromium-browser. About 
# Other
#   [X] Gimp
sudo apt-get install xfe terminator gimp gthumb gnome-specimen \
gksu vim-gtk geany iceweasel

#chromium-browser


#sudo apt-get -y install wine-bin:i386

