#!/bin/bash

# This script installs apps. It can be run multiple times.

# apt-get                            get help
# apt-cache                          get help
# apt-cache search keyword           search for a package
# apt-cache search .                 list all packages
# apt-file list package_name         display contents of a packahe
# apt-get remove pkg                 remove pkg but leave config in place
# apt-get --purge remove pkg         remove everything
# apt-get update                     get latest info from package repos but does NOT update installed packages
# apt-get upgrade pkg                upgrade pkg
# apt-get upgrade                    upgrade all packages
# apt-get dist-upgrade               upgrade all packages and handle changed dependencies

sudo apt-get update

# X apps.
# Openbox is lightest but menu doesn't update unless you also install
# the (tiny) 'menu' package. XFCE and LXDE are pretty good.
# File Manager
#   [X] xfe is nice! and works well over SSH.
#   [ ] PCManFM is fast
#   [ ] Thunar is OK.
# Terminal
#   [X] Terminator - has solarized theme built-in.
#   [ ] ETerm - very small but a bit "off".
#   [ ] LXTerminal - OK, small, about 3Mb. Has tabs.
#       No real need if you are installing terminator.
#   See https://gist.github.com/XVilka/8346728 for true-color terminals.
# Web browser
#   [X] iceweasel (this is Firefox). About 60Mb.
#   [X] chromium-browser. ?
sudo apt-get install \
     evince \
     galculator \
     gimp \
     git-gui \
     gitk \
     gksu \
     gnome-mahjongg \
     gnome-mines \
     gnome-specimen \
     gthumb \
     iceweasel \
     kdbg \
     kdiff3 \
     menu \
     openbox \
     terminator \
     xfce4 \
     xfe \
     xorg
