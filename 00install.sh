#!/bin/bash

# Sets up the bash environment.
source .bash_functions
f_DetermineOS

# Always copy this. It can get tricky having a file called .gitconfig in the
# repo, because that affects the behaviour of git in this repo!
f_CopyFileWithBackup .gitconfig.master ~/.gitconfig
if f_AtWork; then
    echo "It looks like you are at Landmark, updating ~/.gitconfig..."
    sed -i.bak 's/email = Philip.Daniels1971@gmail.com/email = Philip.Daniels@landmark.co.uk/g' ~/.gitconfig
fi

if [ "$OS" == "linux" ] ; then
  f_Relink ~/repos/dotfiles/.bash_logout ~/.bash_logout
  f_Relink ~/repos/dotfiles/.bash_profile ~/.bash_profile
  f_Relink ~/repos/dotfiles/.bashrc ~/.bashrc
  f_Relink ~/repos/dotfiles/.profile ~/.profile
  f_Relink ~/repos/dotfiles/.gvimrc ~/.gvimrc
  f_Relink ~/repos/dotfiles/.vimrc ~/.vimrc
  f_Relink ~/repos/dotfiles/.screenrc ~/.screenrc
  f_Relink ~/repos/dotfiles/.minttyrc ~/.minttyrc
  f_Relink ~/repos/dotfiles/colors/.dircolors.solarized.ansi-universal ~/.dircolors
  f_Relink ~/repos/dotfiles/.tmux.conf ~/.tmux.conf
else
  # Windows does not support symbolic links so we must copy files into place.
  f_CopyFileWithBackup .bash_logout ~/.bash_logout
  f_CopyFileWithBackup .bash_profile ~/.bash_profile
  f_CopyFileWithBackup .bashrc ~/.bashrc
  f_CopyFileWithBackup .profile ~/.profile
  f_CopyFileWithBackup .gvimrc ~/.gvimrc
  f_CopyFileWithBackup .vimrc ~/.vimrc
  f_CopyFileWithBackup .screenrc ~/.screenrc
  f_CopyFileWithBackup .minttyrc ~/.minttyrc
  f_CopyFileWithBackup colors/.dircolors.solarized.ansi-universal ~/.dircolors
  f_CopyFileWithBackup .tmux.conf ~/.tmux.conf
fi

echo "Installation complete."
echo "Remember to check your ~/.gitconfig email address and proxy by typing f_GitShowConfig."

