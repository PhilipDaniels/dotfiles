#!/bin/bash

# This script "sets up" dotfiles itself. It does not install any packages via
# apt-get, it just installs or symlinks files as appropriate.
# It works on Linux and Cygwin.

# Determine the location of this script.
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Sets up the bash environment.
source $DIR/.bash_functions
f_DetermineOS
f_DetermineLinuxDistro

f_Install $DIR/.bash_logout ~/.bash_logout
f_Install $DIR/.bash_profile ~/.bash_profile
f_Install $DIR/.bashrc ~/.bashrc
f_Install $DIR/.profile ~/.profile
f_Install $DIR/.tmux.conf ~/.tmux.conf
f_Install $DIR/.tigrc ~/.tigrc

######################################################################################
# Setup Git.
# Always copy this. It can get tricky having a file called .gitconfig in the
# repo, because that affects the behaviour of git in this repo!
f_CopyFileWithBackup $DIR/.gitconfig.master ~/.gitconfig
if f_AtWork; then
    echo "It looks like you are at Landmark, updating ~/.gitconfig to use your work email and enable the proxy..."
    sed -i.bak 's/email = Philip.Daniels1971@gmail.com/email = Philip.Daniels@landmark.co.uk/g' ~/.gitconfig
    f_GitSetProxy
else
    echo "You are at home, un-setting the Git proxy server"
    f_GitUnsetProxy
fi

cat $DIR/.gitconfig.linux >> ~/.gitconfig

echo "Installation complete."
