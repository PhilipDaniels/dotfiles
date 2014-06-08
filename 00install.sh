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

f_GetTimestamp()
{
    # Return the current date and time as a string suitable for timestamping.
    date +"%Y-%m-%d.%I.%M.%S"
}

f_BackupFile()
{
    # Backs up a file using the current date-time to form a timestamp
    # which is appeneded to the filename.
    # $1: name of the file to backup.
    local filename=$1
    local ts=`f_GetTimestamp`
    local filenameBak="$filename.$ts"

    if [ -f $filename ]; then
        cp $filename $filenameBak
    fi
}

f_CopyFileWithBackup()
{
    # Copies a file to a destination, but backs up the destination
    # first file if it exists, to prevent overwriting.
    # $1: name of the file to copy.
    # $2: the destination filename.
    local srcFile=$1
    local destFile=$2
    f_BackupFile $2
    cp $srcFile $destFile
}

f_Relink()
{
    # Ensures a link exists. If the source is a file a backup is taken,
    # otherwise it is just deleted, then a new link from src to target
    # is established.
    # $1: link target
    # $2: link source
    local target=$1
    local src=$2

    if [ -e $src ]; then
        if [ -f $src ]; then
            f_BackupFile $src
        fi
        rm -f $src
    fi

    ln -s $target $src
}

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
  f_CopyFileWithBackup colors/.minttyrc ~/.minttyrc
  f_CopyFileWithBackup colors/.minttyrc.solarized.dark ~/.minttyrc.solarized.dark
  f_CopyFileWithBackup colors/.minttyrc.solarized.light ~/.minttyrc.solarized.light
  f_CopyFileWithBackup colors/.dircolors.solarized.ansi-universal ~/.dircolors
fi

