#!/bin/bash

. ~/repos/dotfiles/.bash_functions

# To execute this script when you login, create a shortcut in your
# Windows startup folder, typically at
#   C:\Users\Phil\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup
# with the properties
#   C:\cygwin\bin\mintty.exe -e /bin/bash -l -c "~/repos/dotfiles/bin/cygwin_login.sh"


# Start ssh-agent.
# See http://www.funtoo.org/Keychain
if f_AtHome; then
    keychain --eval id_phil
fi


# Start Emacs daemon.
emacs-w32 --daemon
