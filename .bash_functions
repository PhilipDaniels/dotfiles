# vim: set filetype=sh:

# Boolean functions should return 0 for success, following the same
# convention as unix utilities.

echo "***** Running dotfiles/.bash_functions"

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

########################################################################
# Support for setting the terminal color in the Linux virtual console and mintty.
f_SetTerminalColors()
{
    # First set all the colors for our chosen scheme.
    local base03
    local base02
    local base01
    local base00
    local base0
    local base1
    local base2
    local base3
    local yellow
    local orange
    local red
    local magenta
    local violet
    local blue
    local cyan
    local green

    if [ $1 == "SolarizedDark" ]; then
        # The order of these colors matches those on the Solarized home page.
        # at http://ethanschoonover.com/solarized
        base03="002b36"   #   0,  43,  54
        base02="073642"   #   7,  54,  66 
        base01="586e75"   #  88, 110, 117
        base00="657b83"   # 101, 123, 131
        base0="839496"    # 131, 148, 150
        base1="93a1a1"    # 147, 161, 161
        base2="eee8d5"    # 238, 232, 213
        base3="fdf6e3"    # 253, 246, 227
        yellow="b58900"   # 181, 137,   0
        orange="cb4b16"   # 203,  75,  22
        red="dc322f"      # 220,  50,  47
        magenta="d33682"  # 211,  54, 130
        violet="6c71c4"   # 108, 113, 196
        blue="268bd2"     #  38, 139, 210
        cyan="2aa198"     #  42, 161, 152
        green="859900"    # 133, 153,   0
    fi


    # Then issue the appropriate set of escape codes. These vary
    # from terminal to terminal.
    if [ "$TERM" == "linux" ]; then
        echo -en "\e]P8${base03}"   # brblack
        echo -en "\e]P0${base02}"   # black
        echo -en "\e]PA${base01}"   # brgreen
        echo -en "\e]PB${base00}"   # bryellow
        echo -en "\e]PC${base0}"    # brblue
        echo -en "\e]PE${base1}"    # brcyan
        echo -en "\e]P7${base2}"    # white
        echo -en "\e]PF${base3}"    # brwhite
        echo -en "\e]P3${yellow}"   # yellow
        echo -en "\e]P9${orange}"   # brred
        echo -en "\e]P1${red}"      # red
        echo -en "\e]P5${magenta}"  # magenta
        echo -en "\e]PD${violet}"   # brmagenta
        echo -en "\e]P4${blue}"     # blue
        echo -en "\e]P6${cyan}"     # cyan
        echo -en "\e]P2${green}"    # green
    fi

    clear # deal with background artifacting    
}

f_SetSolDark()
{
    f_SetTerminalColors "SolarizedDark"
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


