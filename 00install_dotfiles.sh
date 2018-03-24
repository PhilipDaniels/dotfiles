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

function f_Inst()
{
    local src=$1
    local dest=$2
    f_Relink $src $dest
}


# Always copy this. It can get tricky having a file called .gitconfig in the
# repo, because that affects the behaviour of git in this repo!
f_CopyFileWithBackup $DIR/.gitconfig.master ~/.gitconfig
if f_AtWork; then
    echo "It looks like you are at Landmark, updating ~/.gitconfig..."
    sed -i.bak 's/email = Philip.Daniels1971@gmail.com/email = Philip.Daniels@landmark.co.uk/g' ~/.gitconfig
fi

# Ensure we have the right git client.
if [ "$OS" == "cygwin" ] ; then
    echo "This is Cygwin, updating ~/.gitconfig to set git core.editor to emacsclient-w32"
    sed -i.bak 's/editor = emacsclient/editor = emacsclient-w32.exe/g' ~/.gitconfig
fi

if f_AtWork; then
    echo "You are at work, setting the Git proxy server"
    f_GitSetProxy
else
    echo "You are at home, un-setting the Git proxy server"
    f_GitUnsetProxy
fi

# Do Cygwin specific things.
if [ "$OS" == "cygwin" ] ; then
    f_Inst $DIR/colors/mintty-themes/SolarizedDark ~/.minttyrc
    f_CopyFileWithBackup $DIR/ConEmu.xml ~/AppData/Roaming/ConEmu.xml

    if [ ! -f /bin/apt-cyg ] ; then
        echo "Downloading apt-cyg to /bin"
        lynx -source https://raw.githubusercontent.com/transcode-open/apt-cyg/master/apt-cyg > /bin/apt-cyg
        chmod ugo+rx /bin/apt-cyg
    fi
fi


f_Inst $DIR/.bash_logout ~/.bash_logout
f_Inst $DIR/.bash_profile ~/.bash_profile
f_Inst $DIR/.bashrc ~/.bashrc
f_Inst $DIR/.profile ~/.profile
# f_Inst $DIR/colors/.dircolors.solarized.ansi-universal ~/.dircolors
f_Inst $DIR/.tmux.conf ~/.tmux.conf

# We use Spacemacs now, which we get automatically by not having a .emacs file, after
# following the Spacemacs install instructions at https://github.com/syl20bnr/spacemacs#default-installation
#f_Inst $DIR/emacs/emacs.el ~/.emacs
rm -f ~/.emacs
f_Inst $DIR/.spacemacs ~/.spacemacs


if [ -f /etc/debian_version ]; then
    if [ "$DISTRO" == "DEBIAN" ] ; then
        # We are running on Debian, this file improves font rendering considerably,
        # but it is not necessary for Ubuntu, Mint etc.
        echo "Installing ~/.fonts.conf because this is Debian (note this may be obsolete now)"
        f_Inst $DIR/.fonts.conf ~/.fonts.conf
    fi
fi


echo "Installation complete."
