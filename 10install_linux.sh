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
sudo apt-get dist-upgrade

# Non-X stuff. The fonts don't require X. libssl-dev and apache2-utils
# are required to compile node. Includes dependencies for compiling
# git and Emacs 25 from source.
sudo apt-get install \
     apache2-utils \
     apt-file \
     asciidoc \
     autoconf \
     automake \
     bc \
     bison \
     build-essential \
     cgdb \
     checkinstall \
     cifs-utils \
     cmake \
     cscope \
     curl \
     dblatex \
     docbook2x \
     docbook-utils \
     dos2unix \
     emacs \
     exuberant-ctags \
     flex \
     fonts-droid \
     fonts-inconsolata \
     fonts-jura \
     fonts-linuxlibertine \
     fonts-quattrocento \
     fortune \
     g++ \
     gcc \
     gdb \
     gettext \
     git \
     git-core \
     git-doc \
     gperf \
     htop \
     libcurl4-gnutls-dev \
     libdbus-1-dev \
     libexpat1-dev gettext \
     libgif-dev \
     libgtk2.0-dev \
     libjpeg-dev \
     libm17n-dev \
     libncurses5-dev \
     libotf-dev \
     libpng12-dev \
     librsvg2-dev \
     libssl-dev \
     libtiff-dev \
     libtool \
     libtool-bin \
     libxml2-dev \
     libz-dev \
     links2 \
     make \
     mc \
     ncdu \
     pandoc \
     rsync \
     samba \
     screen \
     smbclient \
     source-highlight \
     ssh \
     strace \
     sudo \
     texinfo \
     tig \
     tmux \
     tree \
     ttf-adf-gillius \
     ttf-adf-ikarius \
     ttf-adf-romande \
     ttf-adf-switzera \
     ttf-adf-verana \
     ttf-bitstream-vera \
     ttf-dejavu \
     ttf-liberation \
     ttf-radisnoir \
     usbutils \
     vim \
     wcd \
     winbind \
     xmlto \
     xorg-dev

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


sudo apt-get dist-upgrade
