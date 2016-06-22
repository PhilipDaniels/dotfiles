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
     cabextract \
     cgdb \
     checkinstall \
     cifs-utils \
     cmake \
     cscope \
     curl \
     dblatex \
     docbook \
     docbook2x \
     docbook-utils \
     dos2unix \
     emacs \
     exuberant-ctags \
     flex \
     font-manager \
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
