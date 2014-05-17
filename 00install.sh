#!/bin/bash

# Sets up the bash environment.

f_DetermineOS()
{
  case "$OSTYPE" in
    cygwin*)  OS="cygwin" ;;
    linux*)   OS="linux" ;;
    msys*)    OS="msys" ;;
    *)        OS="unknown: $OSTYPE"  ;;
  esac
}
f_DetermineOS


# Nuke any existing files or links.
rm -f ~/.bash_functions ~/.bash_logout ~/.bash_profile \
  ~/.bashrc ~/.profile ~/.gitconfig ~/.gvimrc ~/.vimrc \
  ~/.dircolors


if [ "$OS" == "linux" ] ; then
  ln -s ~/repos/dotfiles/.bash_functions ~/.bash_functions
  ln -s ~/repos/dotfiles/.bash_logout ~/.bash_logout
  ln -s ~/repos/dotfiles/.bash_profile ~/.bash_profile
  ln -s ~/repos/dotfiles/.bashrc ~/.bashrc
  ln -s ~/repos/dotfiles/.profile ~/.profile
  ln -s ~/repos/dotfiles/.gvimrc ~/.gvimrc
  ln -s ~/repos/dotfiles/.vimrc ~/.vimrc
  ln -s ~/repos/dotfiles/.dircolors.solarized ~/.dircolors
else
  # Windows does not support symbolic links so we must copy files into place.
  cp .bash_functions ~/.bash_functions
  cp .bash_logout ~/.bash_logout
  cp .bash_profile ~/.bash_profile
  cp .bashrc ~/.bashrc
  cp .profile ~/.profile
  cp .gvimrc ~/.gvimrc
  cp .vimrc ~/.vimrc
  #cp .dircolors.solarized ~/.dircolors
fi


# Always copy this. It can get tricky having a file called
# .gitconfig in the repo, because that affects the behaviour
# of git in this repo!
cp .gitconfig.master ~/.gitconfig
 
