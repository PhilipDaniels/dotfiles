# -*- mode: shell-script -*-

# See https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# Start ssh-agent. This writes a bash script into ~/.keychain and sources it,
# which ensures that other processes know that ssh-agent is running.
# See http://www.funtoo.org/Keychain
# and https://thomaswabner.wordpress.com/2009/11/06/using-keychain-under-cygwin/
# There must be a corresponding call in .bashrc to source the existing file.
#if f_AtHome; then
    #eval `keychain --eval id_phil`
#fi

# Keep functions in separate files.
. ~/repos/dotfiles/.bash_functions
f_DetermineOS
f_DetermineLinuxDistro
f_IsRoot

# Start ssh-agent. This writes a bash script into ~/.keychain and sources it,
# which ensures that other processes know that ssh-agent is running.
# See http://www.funtoo.org/Keychain
# and https://thomaswabner.wordpress.com/2009/11/06/using-keychain-under-cygwin/
# This should be run first time from cygwin_login.sh so it should be a no-op
# when run in .bashrc, except for bringing in the environment variables.
if f_AtHome; then
    if [ "$OS" == "cygwin" ] || [ "$OS" == "msys" ]; then
        eval `keychain --quiet --eval id_phil`
    fi
fi


# Ensure that Git sets core.fileMode to false whenever I cd into a repo directory.
# See http://stackoverflow.com/questions/12457910/how-do-i-prevent-git-on-cygwin-to-set-core-filemode-true
if [ "$OS" == "cygwin" ] || [ "$OS" == "msys" ]; then
    PROMPT_COMMAND=f_PromptCommand
fi

f_PromptCommand()
{
    [ -d .git -a ! -g .git/config ] || return

    # git config core.fileMode 0
    # echo core.fileMode was set to false

    # And also ensure that my personal email address is used for repos
    # underneath ~/repos (at work only).
    if [ "${PWD##/c/Users/pdaniels/repos}" != "${PWD}" ] ; then
        f_GitPersonalEmail
        echo Configured user.email to my personal address
    fi

    chmod +s .git/config
}

#if f_IsCmd "fortune"; then
#    echo
#    fortune -a ~/repos/dotfiles/fortunes ~/repos/dotfiles/fortunes-dune
#    echo
#fi

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
# f_AddToPath "$HOME/bin/p4merge/bin"
# f_AddToPath "$HOME/repos/dotfiles/fancontrol"
# f_AddToPath "$HOME/repos/dotfiles/bin"
f_AddToPath "$HOME/.cargo/bin"
# f_AddToPath "/data/bin"
# f_AddToPath "/data/bin/dotnet"

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

# Remove credential cache files older than 12 hours, which essentially means
# that they date from yesterday (the file may get updated during the day).
# This only affects work, at home I use ssh.
find ~/.git-credential-cache -mmin +720 -delete 2> /dev/null

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
#if [ -x /usr/bin/dircolors ]; then
#    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
#fi

# Emacs setup. NO_AT_BRIDGE suppresses a message from GUI Emacs when starting
# under Cygwin. n.b. EDITOR is not used by my Git setup, the editor is set
# explicitly in my ~/.gitconfig.
export NO_AT_BRIDGE=1

ALTERNATE_EDITOR=""
if [ "$OS" == "cygwin" ]; then
    export EDITOR="emacsclient-w32 -q"
    alias e='emacs-w32'
    alias ec='emacsclient-w32 -q'
else
    export EDITOR="emacsclient -q"
    alias e='emacs'
    alias ec='emacsclient -q'
fi

alias emacslatest='~/repos/emacs/src/emacs'

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

if [ "$OS" == "cygwin" ]; then
    # Always export this so that an X server started from one Cygwin terminal
    # is available from another.
    export DISPLAY=:0.0

    # This will start an X server on Cygwin without displaying any startup windows.
    # If this is not working, you probably forgot to install the "xinit" package.
    # http://x.cygwin.com/docs/faq/cygwin-x-faq.html#q-whereis-startxwin-bat
    alias runx="run xwin -multiwindow -listen tcp"

    # To fix 'Failed to connect to server' errors.
    alias tmux="rm -rf /tmp/tmux* && tmux"
fi

if [ "$OS" == "winbash" ]; then
    export DISPLAY=:0.0
fi

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
