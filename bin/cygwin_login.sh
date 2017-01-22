#!/bin/bash

. ~/repos/dotfiles/.bash_functions

# To execute this script when you login, create a shortcut in your
# Windows startup folder, typically at
#   C:\Users\Phil\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup
# with the properties
#   C:\cygwin\bin\mintty.exe -e /bin/bash -l -c "~/repos/dotfiles/bin/cygwin_login.sh"


# Start ssh-agent. This writes a bash script into ~/.keychain and sources it,
# which ensures that other processes know that ssh-agent is running.
# See http://www.funtoo.org/Keychain
# and https://thomaswabner.wordpress.com/2009/11/06/using-keychain-under-cygwin/
if f_AtHome; then
    eval `keychain --eval id_phil`
fi


# Start Emacs daemon.
emacs-w32 --daemon
