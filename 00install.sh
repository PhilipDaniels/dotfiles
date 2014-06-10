#!/bin/bash

# Sets up the bash environment.
source .bash_functions

# Always copy this. It can get tricky having a file called
# .gitconfig in the repo, because that affects the behaviour
# of git in this repo!
f_CopyFileWithBackup .gitconfig.master ~/.gitconfig

if [ "$OS" == "linux" ] ; then
  f_Relink ~/repos/dotfiles/.bash_logout ~/.bash_logout
  f_Relink ~/repos/dotfiles/.bash_profile ~/.bash_profile
  f_Relink ~/repos/dotfiles/.bashrc ~/.bashrc
  f_Relink ~/repos/dotfiles/.profile ~/.profile
  f_Relink ~/repos/dotfiles/.gvimrc ~/.gvimrc
  f_Relink ~/repos/dotfiles/.vimrc ~/.vimrc
  f_Relink ~/repos/dotfiles/.screenrc ~/.screenrc
  f_Relink ~/repos/dotfiles/colors/.minttyrc ~/.minttyrc
  f_Relink ~/repos/dotfiles/colors/.minttyrc.solarized.dark ~/.minttyrc.solarized.dark
  f_Relink ~/repos/dotfiles/colors/.minttyrc.solarized.light ~/.minttyrc.solarized.light
  f_Relink ~/repos/dotfiles/colors/.dircolors.solarized.ansi-universal ~/.dircolors
else
  # Windows does not support symbolic links so we must copy files into place.
  f_CopyFileWithBackup .bash_logout ~/.bash_logout
  f_CopyFileWithBackup .bash_profile ~/.bash_profile
  f_CopyFileWithBackup .bashrc ~/.bashrc
  f_CopyFileWithBackup .profile ~/.profile
  f_CopyFileWithBackup .gvimrc ~/.gvimrc
  f_CopyFileWithBackup .vimrc ~/.vimrc
  f_CopyFileWithBackup .screenrc ~/.screenrc
  f_CopyFileWithBackup colors/.minttyrc ~/.minttyrc
  f_CopyFileWithBackup colors/.minttyrc.solarized.dark ~/.minttyrc.solarized.dark
  f_CopyFileWithBackup colors/.minttyrc.solarized.light ~/.minttyrc.solarized.light
  f_CopyFileWithBackup colors/.dircolors.solarized.ansi-universal ~/.dircolors
fi

echo "Installation complete."

