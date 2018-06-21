#!/bin/bash

# This script installs apps. It can be run multiple times.

# apt-get                            get help
# apt-cache                          get help
# apt-cache search keyword           search for a package
# apt-cache search .                 list all packages
# apt-file list package_name         display contents of a package
# apt-get remove pkg                 remove pkg but leave config in place
# apt-get --purge remove pkg         remove everything
# apt-get update                     get latest info from package repos but does NOT update installed packages
# apt-get upgrade pkg                upgrade pkg
# apt-get upgrade                    upgrade all packages
# apt-get dist-upgrade               upgrade all packages and handle changed dependencies

sudo apt-get update

## One-off commands to install Emacs 25 from PPA.
# add-apt-repository ppa:ubuntu-elisp
# apt-get update
# apt-get install emacs-snapshot

# Conflicts
# libcurl4-openssl-dev \


# libssl-dev and apache2-utils are required to compile node.
# Includes dependencies for compiling git and Emacs 25 from source.
sudo apt-get install \
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
colorgcc \
cscope \
curl \
dblatex \
docbook docbook2x docbook-utils \
dos2unix \
doublecmd-gtk \
emacs \
exuberant-ctags \
flex \
font-manager \
fonts-inconsolata fonts-jura fonts-linuxlibertine fonts-quattrocento \
fonts-noto fonts-noto-hinted fonts-noto-mono fonts-noto-unhinted fonts-powerline \
fortune fortunes fortune-mod \
g++ gcc gdb \
galculator \
geany \
gedit \
gettext \
gimp \
git git-core git-doc git-gui gitk \
gksu \
gnome-mahjongg gnome-mines gnome-specimen \
gperf \
gthumb \
htop \
jed \
kate \
kdbg \
kdiff3 \
libc6-dev \
libcurl4-gnutls-dev \
libdbus-1-dev \
libexpat1-dev \
libgif-dev \
libgtk2.0-dev \
libice-dev \
libjpeg-dev \
libjpeg-turbo8-dev \
libm17n-dev \
libncurses5-dev \
libotf-dev \
libpng-dev \
libpng12-dev \
librsvg2-dev \
libsm-dev \
libssl-dev \
libtiff-dev \
libtiff5-dev \
libtool libtool-bin \
libx11-dev \
libxext-dev \
libxi-dev \
libxml2-dev \
libxmu-dev \
libxmuu-dev \
libxpm-dev \
libxrandr-dev \
libxt-dev \
libxtst-dev \
libxv-dev \
libz-dev \
links2 \
m4 \
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
terminator \
texinfo \
tig \
tmux \
tree \
ttf-adf-gillius ttf-adf-ikarius ttf-adf-romande ttf-adf-switzera ttf-adf-verana ttf-bitstream-vera ttf-dejavu ttf-liberation ttf-radisnoir \
usbutils \
vim vim-doc vim-gtk3 \
wcd \
winbind \
xaw3dg-dev \
x11-apps \
xfe \
xmlto \
xorg-dev \
zlib1g-dev