# See https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Keep aliases and functions in separate files.
if [ -f ~/.bash_functions ]; then 
    . ~/.bash_functions
fi

f_DetermineOS
echo "***** Running dotfiles/.bashrc, this looks like '$OS'"


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
CDPATH=.:~/repos


# include sbin in PATH
if [ -d "/sbin" ] ; then
    PATH="/sbin:$PATH"
fi
if [ -d "/usr/sbin" ] ; then
    PATH="/usr/sbin:$PATH"
fi
# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
if [ -d "$HOME/bin/p4merge/bin" ] ; then
    PATH="$HOME/bin/p4merge/bin:$PATH"
fi



# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|cygwin) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt="yes"

if [ "$force_color_prompt" != "yes" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    	# We have color support; assume it's compliant with Ecma-48
    	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    	# a case would tend to support setf rather than setaf.)
	    color_prompt="yes"
    else
    	color_prompt=""
    fi
else
    color_prompt="yes"
fi

if [ "$color_prompt" == "yes" ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\n$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# This is the prompt in MSysGit:
#    \[\033]0;$MSYSTEM:${PWD//[^[:ascii:]]/?}\007\]\n\[\033[32m\]\u@\h \[\033[33m\]\w$(__git_ps1)\[\033[0m\]\n$

# This is the prompt in Cygwin:
#    \[\e]0;\w\a\]\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$



# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi



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

if ! f_AgentIsRunning; then
    f_AgentLoadEnv
fi

if ! f_AgentIsRunning; then
    f_AgentStart
    ssh-add ~/.ssh/id_phil
elif ! f_AgentHasKeys; then
    ssh-add ~/.ssh/id_phil
fi

unset env


########################################################################
# Do aliases last.
if [ "$OSTYPE" == "cygwin" ] || [ "$OSTYPE" == "msys" ]; then
    # On Windows, setup various folders to be ignored in ls commands.
    # This is to hide garbage in my %UserProfile% folder.
    LSIGNORE="-I NTUSER.DAT\* -I ntuser.dat\* -I AppData\* -I Cookies\*"
    LSIGNORE="$LSIGNORE -I ntuser.ini -I NetHood -I PrintHood -I Searches"

    # Ignore case while completing.
    set completion-ignore-case on
fi

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

alias grep="grep --color"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

alias ls="ls $LSIGNORE -hF --color=auto"
alias ll="ls $LSIGNORE -lA --color=auto"
alias la="ls $LSIGNORE -A --color=auto"
alias l="ls $LSIGNORE -CF --color=auto"

alias more='less'
alias cls='printf "\033c"'

# This will start an X server on Cygwin without displaying any windows.
if [ "$OSTYPE" == "cygwin" ]; then
    alias startcygx="touch ~/.startxwinrc; startxwin.exe; export DISPLAY=:0.0"
fi

if [ "$OSTYPE" == "msys" ]; then
    alias gvim="~/OtherApps/gvim7.4/gvim.exe"
fi
