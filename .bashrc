# See https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Keep aliases and functions in separate files.
if [ -f .bash_aliases ]; then 
    . .bash_aliases
fi
if [ -f .bash_functions ]; then 
    . .bash_functions
fi

DetermineOS
echo "***** Running .bashrc, this looks like '$OS'"


# This is probably not needed.
# EDITOR="vim"

# Don't put duplicate lines in the history. See bash(1) for more options
HISTCONTROL=ignoredups:ignorespace

# Append to the history file, don't overwrite it
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# Check window size after each commadn and updates LINES and COLUMNS.
shopt -s checkwinsize

# List of directories to be searched by the cd command.
CDPATH=.:~:~/../repos

export PATH=$HOME/local/bin:$PATH

########################################################################
# Setup ssh-agent.
# Run the keychain program to cache ssh keys. Keychain comes with Cygwin
# and is just a shell script which you should copy into the PortableGit
# folder. MSysGit and Cygwin will interfere with each other so you need
# to give them separate folders.
#if IsCygwin; then
#    keychain --dir ~/.keychain-cygwin --eval id_phil
#else
#    keychain --eval id_phil
#fi

# Note: ~/.ssh/environment should not be used, as it already has a
# different purpose in SSH.
env=~/.ssh/agent.env

if ! agent_is_running; then
    agent_load_env
fi

if ! agent_is_running; then
    agent_start
    ssh-add ~/.ssh/id_phil
elif ! agent_has_keys; then
    ssh-add ~/.ssh/id_phil
fi

unset env
########################################################################
