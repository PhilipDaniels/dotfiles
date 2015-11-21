#!/bin/bash

# Determine the location of this script.
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Sets up the bash environment.
source $DIR/.bash_functions
f_DetermineOS

# Always copy this. It can get tricky having a file called .gitconfig in the
# repo, because that affects the behaviour of git in this repo!
f_CopyFileWithBackup $DIR/.gitconfig.master ~/.gitconfig
if f_AtWork; then
    echo "It looks like you are at Landmark, updating ~/.gitconfig..."
    sed -i.bak 's/email = Philip.Daniels1971@gmail.com/email = Philip.Daniels@landmark.co.uk/g' ~/.gitconfig
fi

# Download apt-cyg if it does not exist
if [ "$OS" == "cygwin" ] ; then
    f_CopyFileWithBackup $DIR/ConEmu.xml ~/AppData/Roaming/ConEmu.xml

    if [ ! -f /bin/apt-cyg ] ; then
        echo "Downloading apt-cyg to /bin"
        lynx -source https://raw.githubusercontent.com/transcode-open/apt-cyg/master/apt-cyg > /bin/apt-cyg
        chmod ugo+rx /bin/apt-cyg
    fi
fi

function f_Inst()
{
    local src=$1
    local dest=$2

    if [ "$OS" == "linux" ] ; then
	f_Relink $src $dest
    else
	f_CopyFileWithBackup $src $dest
    fi
}

f_Inst $DIR/.bash_logout ~/.bash_logout
f_Inst $DIR/.bash_profile ~/.bash_profile
f_Inst $DIR/.bashrc ~/.bashrc
f_Inst $DIR/.gvimrc ~/.gvimrc
f_Inst $DIR/.profile ~/.profile
f_Inst $DIR/.screenrc ~/.screenrc
f_Inst $DIR/.tmux.conf ~/.tmux.conf
f_Inst $DIR/.vimrc ~/.vimrc
f_Inst $DIR/colors/.dircolors.solarized.ansi-universal ~/.dircolors
f_Inst $DIR/colors/mintty-themes/SolarizedDark ~/.minttyrc
f_Inst $DIR/emacs/.emacs ~/.emacs

# if [ ! -f /usr/share/fonts/TTF/Cousine-Regular.ttf ] ; then
#     echo "Installing mono fonts..."
#     cp $DIR/fonts/Mono/*.ttf /usr/share/fonts/TTF
# fi

echo "Installation complete."
