# -*- mode: shell-script -*-

# See https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

. ~/repos/dotfiles/.bash_controlcodes
. ~/repos/dotfiles/.bash_functions
. ~/repos/dotfiles/.bash_prompt

f_DetermineOS
f_DetermineLinuxDistro
f_IsRoot

# This environment variable should only be set on Windows, to plink.exe.
# If set on Cywgin or WSL it interferes with git cloning operations.
unset GIT_SSH

# See bash(1) for details.
HISTCONTROL=ignoreboth      # Don't put duplicate lines or lines starting with space in the history.  
HISTSIZE=1000               # For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTFILESIZE=2000
shopt -s histappend         # Append to the history file, don't overwrite it

# Check window size after each commadn and updates LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# Add these folders to the path (if they exist).
f_AddToPath "/sbin"
f_AddToPath "/usr/sbin"
f_AddToPath "$HOME/bin"
f_AddToPath "$HOME/.cargo/bin"
f_AddToPath "$HOME/.local/bin"

export VISUAL=vim
export EDITOR=vim

# Start ssh-agent. This writes a bash script into ~/.keychain and sources it,
# which ensures that other processes know that ssh-agent is running. This means
# you should only ever have to type in your passphrase once.
# See http://www.funtoo.org/Keychain
# and https://thomaswabner.wordpress.com/2009/11/06/using-keychain-under-cygwin/
# There must be a corresponding call in .bashrc to source the existing file.
# First, find all the private key files that are installed in the directory.
KeyFiles=`find ~/.ssh -type f ! -name "*.*" -name "id*"`
eval `keychain --quiet --eval "$KeyFiles"`

# Enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# Setup nvm (Node Version Manager) to use a folder in my home directory.
if [ -s ~/.nvm/nvm.sh ]; then
    NVM_DIR=~/.nvm
    source ~/.nvm/nvm.sh
fi

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


if [ "$OS" == "wsl" ]; then
    export DISPLAY=:0.0
fi

# Liquidprompt is rather slow on my work machine, though tolerable.
# It's fine on real Linux though and my home PC though.
if [ -f ~/repos/liquidprompt/liquidprompt ] ; then
    source ~/repos/liquidprompt/liquidprompt
fi

if f_IsCmd "fasd"; then
    eval "$(fasd --init auto)"
fi

########################################################################

alias grep="grep --color"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias ls="ls -hF --color=auto"
alias ll="ls -l --color=auto"
alias la="ls -lA --color=auto"
alias lc="ls -A --color=auto"
alias l="ls -CF --color=auto"
alias lt="ls -lA --color=auto --sort=time"
alias more='less'
alias cls='printf "\033c"'
alias quit='exit'
alias wcd='wcd -q'
alias s='git status'
alias b='git branch -a -vv'
alias bs='git branch -a -vv;echo "";git status'
alias q='git status;echo "";git branch -a -vv;echo "";git l -10'
alias co='git checkout'
alias cob='git checkout -b'
alias cod='git checkout develop'
alias com='git checkout master'
alias gfm='git config --local core.fileMode false'
alias gp='git pull'

########################################################################
#if f_IsCmd "fortune"; then
#    echo
#    fortune -a ~/repos/dotfiles/fortunes ~/repos/dotfiles/fortunes-dune
#    echo
#fi

# List of directories to be searched by the cd command.
# CDPATH=.:~/repos

# dircolors only affects the output of the "ls" command.
# Typically, my dotfiles installs the "ansi-universal" dircolors database, which
# requires the terminal to have been configured with solarized colors but will
# fall back to reasonable defaults if not. n.b. MSysGit does not provide dircolors,
# but we seem to get some highlighting anyway.
#if [ -x /usr/bin/dircolors ]; then
#    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
#fi

# Remove credential cache files older than 24 hours, which essentially means
# that they date from yesterday (the file may get updated during the day).
# This only affects work, at home I use ssh.
# find ~/.git-credential-cache -mmin +1440 -delete 2> /dev/null
