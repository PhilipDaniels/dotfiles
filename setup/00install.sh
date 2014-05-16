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
    ~/.bashrc ~/.profile ~/.gitconfig ~/.gvimrc ~/.vimrc


if [ "$OS" == "linux" ] ; then
  ln -s ~/repos/dotfiles/.bash_functions ~/.bash_functions
  ln -s ~/repos/dotfiles/.bash_logout ~/.bash_logout
  ln -s ~/repos/dotfiles/.bash_profile ~/.bash_profile
  ln -s ~/repos/dotfiles/.bashrc ~/.bashrc
  ln -s ~/repos/dotfiles/.profile ~/.profile
  # ln -s ~/repos/dotfiles/.dircolors ~/.dircolors
  ln -s ~/repos/dotfiles/.gitconfig ~/.gitconfig
  ln -s ~/repos/dotfiles/.gvimrc ~/.gvimrc
  ln -s ~/repos/dotfiles/.vimrc ~/.vimrc
else
  # Windows does not support symbolic links so we must copy files into place.
  cp ../.bash_functions ~/.bash_functions
  cp ../.bash_logout ~/.bash_logout
  cp ../.bash_profile ~/.bash_profile
  cp ../.bashrc ~/.bashrc
  cp ../.profile ~/.profile
  cp ../.gitconfig ~/.gitconfig
  cp ../.gvimrc ~/.gvimrc
  cp ../.vimrc ~/.vimrc
fi

 
