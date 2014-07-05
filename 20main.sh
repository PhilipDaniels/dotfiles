#!/bin/bash

# This script installs apps. It can be run multiple times.
# To see the contents of a package do 'apt-file list package_name'

sudo apt-get update

# Non-X stuff. The fonts don't require X. libssl-dev and apache2-utils
# are required to compile node.
sudo apt-get install samba winbind smbclient cifs-utils vim cscope \
automake bc bison build-essential checkinstall libtool rsync wcd \
flex exuberant-ctags g++ gcc gdb gettext git git-core tig htop make cmake \
strace sudo usbutils links2 git git-doc ssh apt-file curl vim cgdb \
libssl-dev apache2-utils mc ncdu tree dos2unix screen fortune tmux \
ttf-bitstream-vera ttf-dejavu fonts-droid ttf-liberation \
fonts-inconsolata fonts-jura fonts-linuxlibertine fonts-quattrocento \
ttf-adf-gillius ttf-adf-ikarius ttf-adf-romande ttf-adf-switzera \
ttf-adf-verana ttf-radisnoir

# X core.
#   Window Manager
#     [X] Openbox is lightest but menu doesn't update unless you also
#         install the (tiny) 'menu' package.
#     [ ] XFCE and LXDE are pretty good.
sudo apt-get install xorg openbox menu
#sudo apt-get install xfce4



# X apps. 
# File Manager
#   [X] xfe is nice! and works well over SSH.
#   [ ] PCManFM is fast
#   [ ] Thunar is OK.
# Text Editor
#   [X] vim-gtk
#   [X] geany, small about 10Mb. Might as well have.
# Diff tool
#   [ ] P4Merge
#   [ ] KDiff3. Cross platform, but brings in 100Mb of KDE libs.
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
#   [X] gnome-mines
sudo apt-get install xfe vim-gtk geany terminator \
iceweasel gimp gitk git-gui \
gthumb gnome-specimen gksu galculator evince gnome-mines \
gnome-mahjongg

#chromium-browser


#sudo apt-get -y install wine-bin:i386

