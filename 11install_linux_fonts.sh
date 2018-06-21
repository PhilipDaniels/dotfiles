#!/bin/bash

# This script installs the Microsoft Office 2007 fonts such as
# Consolas and Calibri.

# The 10 script does this already:
# sudo apt-get install font-manager cabextract

# 21 Jun 2018: See https://github.com/powerline/fonts
# for fonts which are now in Debian, plus another install script.
# Also https://github.com/ryanoasis/nerd-fonts might be of interest.

TMPDIR=fonttemp
DESTDIR=/usr/local/share/fonts
DOCACHE=0

# #############################################################################################
# My original approach.

# Install the Microsoft fonts.
if [ -f $DESTDIR/CONSOLA.TTF ] ; then
    echo "The Microsoft Office 2007 fonts (Consolas, Calibri etc.) are already in $DESTDIR"
else
    set -x

    rm -rf $TMPDIR
    mkdir $TMPDIR
    cd $TMPDIR
    wget http://download.microsoft.com/download/E/6/7/E675FFFC-2A6D-4AB0-B3EB-27C9F8C8F696/PowerPointViewer.exe
    cabextract -L -F ppviewer.cab PowerPointViewer.exe
    cabextract ppviewer.cab

    sudo cp *.TTF $DESTDIR
    sudo chmod -R 777 $DESTDIR

    cd ..
    rm -rf $TMPDIR
    set +x
    DOCACHE=1
fi


# Install the Chris Simpkins font collection.
if [ -f $DESTDIR/AverageMono.ttf ]; then
    echo "The Chris Simpkins Codeface fonts are already installed in $DESTDIR"
else
    set -x
    rm -rf $TMPDIR
    mkdir $TMPDIR
    cd $TMPDIR
    wget https://github.com/chrissimpkins/codeface/releases/download/font-collection/codeface-fonts.zip
    unzip codeface-fonts.zip
    sudo find . -type f -iname '*.ttf' -exec cp '{}' $DESTDIR \;
    sudo chmod -R 777 $DESTDIR
    cd ..
    rm -rf $TMPDIR
    set +x
    DOCACHE=1
fi


if [ "$DOCACHE" == "1" ]; then
    set -x
    fc-cache -fv
    sudo fc-cache -fv
    set +x
fi

rm -rf $TMPDIR

# #############################################################################################
# Copied from the SpaceVim installation script.

# download_font {{{
download_font () {
  url="https://raw.githubusercontent.com/wsdjeg/DotFiles/master/local/share/fonts/$1"
  path="$HOME/.local/share/fonts/$1"
  if [[ -f "$path" ]]
  then
    success "Downloaded $1"
  else
    info "Downloading $1"
    curl -s -o "$path" "$url"
    success "Downloaded $1"
  fi
}

# }}}

# install_fonts {{{
install_fonts () {
  if [[ ! -d "$HOME/.local/share/fonts" ]]; then
    mkdir -p $HOME/.local/share/fonts
  fi
  download_font "DejaVu Sans Mono Bold Oblique for Powerline.ttf"
  download_font "DejaVu Sans Mono Bold for Powerline.ttf"
  download_font "DejaVu Sans Mono Oblique for Powerline.ttf"
  download_font "DejaVu Sans Mono for Powerline.ttf"
  download_font "DroidSansMonoForPowerlinePlusNerdFileTypesMono.otf"
  download_font "Ubuntu Mono derivative Powerline Nerd Font Complete.ttf"
  download_font "WEBDINGS.TTF"
  download_font "WINGDNG2.ttf"
  download_font "WINGDNG3.ttf"
  download_font "devicons.ttf"
  download_font "mtextra.ttf"
  download_font "symbol.ttf"
  download_font "wingding.ttf"
  info "Updating font cache, please wait ..."
  if [ $System == "Darwin" ];then
    if [ ! -e "$HOME/Library/Fonts" ];then
      mkdir "$HOME/Library/Fonts"
    fi 
    cp $HOME/.local/share/fonts/* $HOME/Library/Fonts/
  else
    fc-cache -fv > /dev/null
    mkfontdir "$HOME/.local/share/fonts" > /dev/null
    mkfontscale "$HOME/.local/share/fonts" > /dev/null
  fi
  success "font cache done!"
}


