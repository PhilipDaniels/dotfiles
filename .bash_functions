# vim: set filetype=sh:

# Boolean functions should return 0 for success, following the same
# convention as unix utilities.

echo ">> Running dotfiles/.bash_functions"

function f_ShowPath()
{
    echo $PATH | tr ':' '\n' | sort -u
}

# Add a directory to the path if and only if it is not already
# in the path and if it exists.
# Usage: f_AddToPath "~/some/folder"
#        f_AddToPath "/c/Users/Phil/something"
function f_AddToPath()
{
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        PATH="$1:$PATH"
    fi
}

f_AptUpdateAndUpgrade() 
{
  sudo apt-get update && \
    sudo apt-get -y dist-upgrade && \
    sudo apt-get -y autoremove && \
    sudo apt-get -y autoclean
}

f_DetermineOS()
{
  case "$OSTYPE" in
    cygwin*)  OS="cygwin" ;;
    linux*)   OS="linux" ;;
    msys*)    OS="msys" ;;
    *)        OS="unknown: $OSTYPE"  ;;
  esac
}

f_IsRoot()
{
    # Determine if you are root, and set two environment variables accordingly.
    if [ $EUID -ne 0 ]; then
        export ISROOT=0
        export ISROOTMSG="You are NOT root."
    else
        export ISROOT=1
        export ISROOTMSG="You are root!"
    fi
}

f_AtHome()
{
    # Determine if I am at home or at work.
    case $HOSTNAME in
        RDL*) false ;;
        *)    true ;;
    esac
}

f_AtWork()
{
    ! f_AtHome
}

f_GetTimestamp()
{
    # Return the current date and time as a string suitable for timestamping.
    date +"%Y-%m-%d.%H%M%S"
}

f_BackupFile()
{
    # Backs up a file using the current date-time to form a timestamp
    # which is appeneded to the filename.
    # $1: name of the file to backup.
    # $2: if 1, turns on quiet mode, does not echo a message.
    local filename=$1
    local quietMode=$2
    local ts=`f_GetTimestamp`
    local filenameBak="${filename}.${ts}.bak"

    if [ -f $filename ]; then
        if [ "$quietMode" != "1" ]; then
            echo "Backing up $filename to $filenameBak"
        fi
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

f_Confirm()
{
    # Ask user a YES/NO question and return 1 if the user replies
    # YES, else return 0.
    # $1: The text of the question to ask.
    local question=$1
    read -p "$question [y/n]: "
    case $(echo $REPLY | tr '[A-Z]' '[a-z]') in
        y|yes) echo "1" ;;
        *)     echo "0" ;;
    esac
}

f_DeleteBackups()
{
    # Deletes backup files in the current directory.
    # First displays the files it thinks are backups and then
    # asks for confirmation. The pattern for backups is the
    # one used by the f_BackupFile function.
    if find . -maxdepth 0 -name "*.bak" -o -name ".*.bak" -print; then
        ls -al *.bak .*.bak 2> /dev/null
        local answer=`f_Confirm "Delete selected files?"`
        if [ "$answer" -eq "1" ]; then
            rm -f *.bak .*.bak
        fi
    else
        echo "No backups matching $pattern found."
    fi
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

f_IsCmd()
{
    # Check to see if a command is installed.
    # $1: the command to check.
    # Returns: 1 if the program is installed, 0 otherwise.
    # Example:   if f_IsCmd "fortune"; then ...
    command -v >&- "$@"
}

f_GoDos()
{
    # Convert all text files in the current folder and any child
    # folders to DOS (CRLF) line endings.
    find . -path ./.git -prune -o -type f -exec unix2dos {} \;
}

f_GoUnix()
{
    # Convert all text files in the current folder and any child
    # folders to Unix (LF) line endings.
    find . -path ./.git -prune -o -type f -exec dos2unix {} \;
}

f_DuplicateFile()
{
    local file=$1
    local x=$2
    local y=$3

    for num in $(seq $x $y)
    do
        cp "$file" "$num$file"
    done
}

f_ShowTerminalColors()
{
    # Display the standard 16 terminal colors.
    echo -e "Color  0 ('Black')          : $F_Black Text $F_Default"
    echo -e "Color  1 ('Red')            : $F_Red Text $F_Default"
    echo -e "Color  2 ('Green')          : $F_Green Text $F_Default"
    echo -e "Color  3 ('Yellow')         : $F_Yellow Text $F_Default"
    echo -e "Color  4 ('Blue')           : $F_Blue Text $F_Default"
    echo -e "Color  5 ('Magenta')        : $F_Magenta Text $F_Default"
    echo -e "Color  6 ('Cyan')           : $F_Cyan Text $F_Default"
    echo -e "Color  7 ('White')          : $F_White Text $F_Default"
    echo -e "Color  8 ('Bright Black')   : $F_BrightBlack Text $F_Default"
    echo -e "Color  9 ('Bright Red')     : $F_BrightRed Text $F_Default"
    echo -e "Color 10 ('Bright Green')   : $F_BrightGreen Text $F_Default"
    echo -e "Color 11 ('Bright Yellow')  : $F_BrightYellow Text $F_Default"
    echo -e "Color 12 ('Bright Blue')    : $F_BrightBlue Text $F_Default"
    echo -e "Color 13 ('Bright Magenta') : $F_BrightMagenta Text $F_Default"
    echo -e "Color 14 ('Bright Cyan')    : $F_BrightCyan Text $F_Default"
    echo -e "Color 15 ('Bright White')   : $F_BrightWhite Text $F_Default"
}

f_GitSetProxy()
{
    export http_proxy=http://rdproxy01:800/
    export https_proxy=http://rdproxy01:800/
}

f_GitUnsetProxy()
{
    unset http_proxy
    unset https_proxy
}

f_GitAuthorRewrite()
{
    local oldemail=$1
    local newemail=$2
    local newname=$3

    if [ -z "$oldemail" ] | [ -z "$newemail" ] | [ -z "$newname" ]; then
        echo "Usage: f_GitAuthorRewrite old.address@email.com new@gmail.com 'New HumanName'"
        return 1
    fi

    local filtercmd="
    OLD_EMAIL=\"$oldemail\"
    CORRECT_EMAIL=\"$newemail\"
    CORRECT_NAME=\"$newname\"
    
    if [ \"\$GIT_COMMITTER_EMAIL\" = \"\$OLD_EMAIL\" ]; then
        export GIT_COMMITTER_NAME=\"\$CORRECT_NAME\"
        export GIT_COMMITTER_EMAIL=\"\$CORRECT_EMAIL\"
    fi
    
    if [ \"\$GIT_AUTHOR_EMAIL\" = \"\$OLD_EMAIL\" ]; then
        export GIT_AUTHOR_NAME=\"\$CORRECT_NAME\"
        export GIT_AUTHOR_EMAIL=\"\$CORRECT_EMAIL\"
    fi
    "

    git filter-branch --env-filter "$filtercmd" --tag-name-filter cat -- --branches --tags
}

f_GitShowConfig()
{
    # Lists critical git configuration which I am always getting wrong
    # when moving from work to home.
    echo -e "${F_Red}Global config:${F_Default}"
    git config --list --global | grep 'user.email\|proxy' | sort
    echo -e "\n${F_Red}Local config (takes priority):${F_Default}"
    git config --list --local | grep 'user.email\|proxy' | sort
}

function gv()
{
    # Favour native Windows gvim when in Windows, even if an X server is
    # running. This is because it understands Windows paths better.
    # If you want the Cygwin gvim just do /bin/gvim.

    local filename="$1"
    if [ "$filename" ]; then
        filename=`cygpath -w "$filename"`
    fi

    "$PORTABLEAPPSROOT/gVimPortable/App/vim/vim74/gvim.exe" -T win32 "$filename"
}





########################################################################
# Support for using ssh-agent because keychain doesn't seem to work
# that well in MSysGit.
# See https://help.github.com/articles/working-with-ssh-key-passphrases
# Note: Don't bother checking SSH_AGENT_PID. It's not used
#       by SSH itself, and it might even be incorrect
#       (for example, when using agent-forwarding over SSH).
f_AgentIsRunning()
{
    if [ "$SSH_AUTH_SOCK" ]; then
        # ssh-add returns:
        #   0 = agent running, has keys
        #   1 = agent running, no keys
        #   2 = agent not running
        ssh-add -l >/dev/null 2>&1 || [ $? -eq 1 ]
    else
        false
    fi
}

f_AgentHasKeys()
{
    ssh-add -l >/dev/null 2>&1
}

f_AgentLoadEnv()
{
    . "$env" >/dev/null
}

f_AgentStart()
{
    (umask 077; ssh-agent >"$env")
    . "$env" >/dev/null
}

f_SetupSSH()
{
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
}

########################################################################
# settitle () 
# { 
#   echo -ne "\e]2;$@\a\e]1;$@\a"; 
# }
# 
# b) function cd_func
# This function defines a 'cd' replacement function capable of keeping, 
# displaying and accessing history of visited directories, up to 10 entries.
# To use it, uncomment it, source this file and try 'cd --'.
# acd_func 1.0.5, 10-nov-2004
# Petar Marinov, http:/geocities.com/h2428, this is public domain
# cd_func ()
# {
#   local x2 the_new_dir adir index
#   local -i cnt
# 
#   if [[ $1 ==  "--" ]]; then
#     dirs -v
#     return 0
#   fi
# 
#   the_new_dir=$1
#   [[ -z $1 ]] && the_new_dir=$HOME
# 
#   if [[ ${the_new_dir:0:1} == '-' ]]; then
#     #
#     # Extract dir N from dirs
#     index=${the_new_dir:1}
#     [[ -z $index ]] && index=1
#     adir=$(dirs +$index)
#     [[ -z $adir ]] && return 1
#     the_new_dir=$adir
#   fi
# 
#   #
#   # '~' has to be substituted by ${HOME}
#   [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"
# 
#   #
#   # Now change to the new dir and add to the top of the stack
#   pushd "${the_new_dir}" > /dev/null
#   [[ $? -ne 0 ]] && return 1
#   the_new_dir=$(pwd)
# 
#   #
#   # Trim down everything beyond 11th entry
#   popd -n +11 2>/dev/null 1>/dev/null
# 
#   #
#   # Remove any other occurence of this dir, skipping the top of the stack
#   for ((cnt=1; cnt <= 10; cnt++)); do
#     x2=$(dirs +${cnt} 2>/dev/null)
#     [[ $? -ne 0 ]] && return 0
#     [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
#     if [[ "${x2}" == "${the_new_dir}" ]]; then
#       popd -n +$cnt 2>/dev/null 1>/dev/null
#       cnt=cnt-1
#     fi
#   done
# 
#   return 0
# }
# 
# alias cd=cd_func


