# -*- mode: shell-script -*-

# See https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Keep functions in separate files.
. ~/repos/dotfiles/.bash_functions
f_DetermineOS
f_IsRoot

# Ensure that Git sets core.fileMode to false whenever I cd into a repo directory.
# See http://stackoverflow.com/questions/12457910/how-do-i-prevent-git-on-cygwin-to-set-core-filemode-true
PROMPT_COMMAND=f_PromptCommand
f_PromptCommand()
{
    [ -d .git -a ! -g .git/config ] || return

    git config core.fileMode 0
    echo core.fileMode was set to false

    # And also ensure that my personal email address is used for repos
    # underneath ~/repos (at work only).
    if [ "${PWD##/c/Users/pdaniels/repos}" != "${PWD}" ] ; then
        f_GitPersonalEmail
        echo Configured user.email to my personal address
    fi

    chmod +s .git/config
}

if f_IsCmd "fortune"; then
    echo
    fortune -a ~/repos/dotfiles/fortunes ~/repos/dotfiles/fortunes-dune
    echo
fi

# Bring in control codes (for setting colors etc.) then configure the prompt.
. ~/repos/dotfiles/.bash_controlcodes
. ~/repos/dotfiles/.bash_prompt

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
# CDPATH=.:~/repos

# Add these folders to the path (if they exist).
f_AddToPath "/sbin"
f_AddToPath "/usr/sbin"
f_AddToPath "$HOME/bin"
f_AddToPath "$HOME/bin/p4merge/bin"

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

# Setup nvm (Node Version Manager) to use a folder in my home directory.
if [ -s ~/.nvm/nvm.sh ]; then
    NVM_DIR=~/.nvm
    source ~/.nvm/nvm.sh
fi


#######################################################################
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

# Remove credential cache files older than 12 hours, which essentially means
# that they date from yesterday (the file may get updated during the day).
# This only affects work, at home I use ssh.
find ~/.git-credential-cache -mmin +720 -delete 2> /dev/null

if f_AtHome; then
    #echo "You are at home, setting up ssh..."
    f_SetupSSH
fi


GIT_PROMPT_ONLY_IN_REPO=1
GIT_PROMPT_FETCH_REMOTE_STATUS=0   # uncomment to avoid fetching remote status


########################################################################
# Do aliases last.
if [ "$OS" == "cygwin" ] || [ "$OS" == "msys" ]; then
    # On Windows, setup various folders to be ignored in ls commands.
    # This is to hide garbage in my %UserProfile% folder.
    LSIGNORE="-I NTUSER.DAT\* -I ntuser.dat\* -I AppData\* -I Cookies\*"
    LSIGNORE="$LSIGNORE -I ntuser.ini -I NetHood -I PrintHood -I Searches"
    LSIGNORE="$LSIGNORE -I Application\ Data -I Contacts -I Local\ Settings"
    LSIGNORE="$LSIGNORE -I Lync\ Recordings -I Saved\ Games"

    # Ignore case while completing.
    set completion-ignore-case on

    if [ $OS == "msys" ]; then
        # MSys does not have a good console, so use this Vim which
        # plays nice with ConEmu.
        alias vim="$PORTABLEAPPSROOT/gVimPortable/App/vim/vim7.4/vim.exe"
    fi
fi


# dircolors only affects the output of the "ls" command.
# Typically, my dotfiles installs the "ansi-universal" dircolors database, which
# requires the terminal to have been configured with solarized colors but will
# fall back to reasonable defaults if not. n.b. MSysGit does not provide dircolors,
# but we seem to get some highlighting anyway.
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# Emacs setup. NO_AT_BRIDGE suppresses a message from GUI Emacs when starting
# under Cygwin. n.b. EDITOR is not used by my Git setup, the editor is set
# explicitly in my ~/.gitconfig.
export NO_AT_BRIDGE=1

# {C}ygwin{E}macs.
# If X is running opens X window, else opens console window.
# Use -nw to force open in the console.
alias ce='emacs'

# {W}indows{E}macs
# Will open a new Windows window. emacs-w32 is also from Cygwin, but runs using
# native Windows fonts and without having to start an X server.
# Use -nw to force open in the console (like normal 'emacs')
alias we='emacs-w32'

# {C}ygwin{E}macs{C}lient
# -c means create a new window.
# -t, -nw or -tty means use current terminal.
alias cec='emacsclient'
alias ec='emacsclient -nw'

# {W}indows{E}macs{C}lient
# With no arg, tries to use current frame.
# -c creates a new frame
# -nw does not work and hangs the main Emacs window. DO NOT USE.
alias wec='emacsclient-w32'


# MSysGit grep does not recognise the --color option.
if [ "$OS" != "msys" ]; then
    alias grep="grep --color"
    alias egrep="egrep --color=auto"
    alias fgrep="fgrep --color=auto"
fi

alias ls="ls $LSIGNORE -hF --color=auto"
alias ll="ls $LSIGNORE -lA --color=auto"
alias la="ls $LSIGNORE -A --color=auto"
alias l="ls $LSIGNORE -CF --color=auto"
alias lt="ls $LSIGNORE -lA --color=auto --sort=time"

alias more='less'
alias cls='printf "\033c"'
alias wcd='wcd -q iicolor'
alias quit='exit'

alias s='git status'
alias b='git branch -a -vv'
alias bs='git branch -a -vv;echo "";git status'
alias q='git status;echo "";git branch -a -vv;echo "";git l -10'
alias co='git checkout'
alias cob='git checkout -b'
alias cod='git checkout develop'
alias com='git checkout master'
alias gfm='git config --local core.fileMode false'

alias wcd='wcd -q'

if [ "$OS" == "cygwin" ]; then
    # Always export this so that an X server started from one Cygwin terminal
    # is available from another.
    export DISPLAY=:0.0

    # This will start an X server on Cygwin without displaying any startup windows.
    # If this is not working, you probably forgot to install the "xinit" package.
    # http://x.cygwin.com/docs/faq/cygwin-x-faq.html#q-whereis-startxwin-bat
    alias runx="run xwin -multiwindow"

    # To fix 'Failed to connect to server' errors.
    alias tmux="rm -rf /tmp/tmux* && tmux"
fi

