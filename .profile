# vim: set filetype=sh:

# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022


# This file is executed when you login via SSH; we follow the principles
# in .bash_profile and defer everything to .bashrc.

# if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f ~/.bashrc ]; then
    	. ~/.bashrc
    fi
fi


