#!/bin/bash

# Sets up the bash environment.

# First nuke any existing files.
rm -f ~/.bash_aliases ~/.bash_functions ~/.bash_logout ~/.bash_profile \
    ~/.bashrc ~/.profile ~/.gitconfig ~/.gvimrc ~/.vimrc ~/.dircolors

# Link them to the files in the repo.
ln -s ~/repos/github/dotfiles/.bash_aliases ~/.bash_aliases
ln -s ~/repos/github/dotfiles/.bash_functions ~/.bash_functions
ln -s ~/repos/github/dotfiles/.bash_logout ~/.bash_logout
ln -s ~/repos/github/dotfiles/.bash_profile ~/.bash_profile
ln -s ~/repos/github/dotfiles/.bashrc ~/.bashrc
ln -s ~/repos/github/dotfiles/.profile ~/.profile
ln -s ~/repos/github/dotfiles/.dircolors ~/.dircolors

# These files need to be copied and then possibly tweaked.
cp ../.gitconfig ~
cp ../.gvimrc ~ 
cp ../.vimrc ~
 
