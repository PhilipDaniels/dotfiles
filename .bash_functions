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
# This is to setup the Linux VT for the solarized theme.
# This makes LinuxVT-vim work with solarized, but the ls colors
# are washed out. Fix by using the .dircolors.solarized theme?
f_SetLinuxTerminalToSolarized()
{
    local base02="073642"
    local red="dc322f"
    local green="859900"
    local yellow="b58900"
    local blue="268bd2"
    local magenta="d33682"
    local cyan="2aa198"
    local base2="eee8d5"
    local base03="002b36"
    local orange="cb4b16"
    local base01="586e75"
    local base00="657b83"
    local base0="839496"
    local violet="6c71c4"
    local base1="93a1a1"
    local base3="fdf6e3"

    echo -en "\e]P0${base02}"   # black
    echo -en "\e]P1${red}"      # red
    echo -en "\e]P2${green}"    # green
    echo -en "\e]P3${yellow}"   # yellow
    echo -en "\e]P4${blue}"     # blue
    echo -en "\e]P5${magenta}"  # magenta
    echo -en "\e]P6${cyan}"     # cyan
    echo -en "\e]P7${base2}"    # white
    echo -en "\e]P8${base03}"   # brblack
    echo -en "\e]P9${orange}"   # brred
    echo -en "\e]PA${base01}"   # brgreen
    echo -en "\e]PB${base00}"   # bryellow
    echo -en "\e]PC${base0}"    # brblue
    echo -en "\e]PD${violet}"   # brmagenta
    echo -en "\e]PE${base1}"    # brcyan
    echo -en "\e]PF${base3}"    # brwhite
    clear                       # for background artifacting
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


