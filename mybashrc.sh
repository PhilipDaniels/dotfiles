# See https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html
#
# This is the new (2021) style of my dotfiles for bash. Everything is in 1 file,
# which is manually sourced (see code below) from the ~/.bashrc which is supplied
# by the distribution. This means we are no longer copying files into the home
# directory.
#
#   if [ -f "$HOME/repos/dotfiles/.mybashrc" ]; then
#       . "$HOME/repos/dotfiles/.mybashrc"
#   fi


# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac


# #############################################################################################################################
# Control codes for bash. These are a mixture of formatting instructions (to
# turn on bold fonts etc.), color instructions, and general control
# instructions.
#
# References
# http://wiki.bash-hackers.org/scripting/terminalcodes
#     "The VT implemented in the Linux kernel supports only 16 colors, and the usual
#     default terminfo entry for TERM=linux defines only 8. There is sometimes an
#     alternate "linux-16color" that you can switch to to get the other 8 colors."
# http://misc.flogisoft.com/bash/tip_colors_and_formatting#terminals_compatibility
# http://ascii-table.com/ansi-escape-sequences.php

# These sequences are formatting instructions. Mnemonic: S = set, R = reset.
R_All='\e[0m'             # Turns off all special formatting. Very handy.
S_Bold='\e[1m'            # Bold, aka "bright".
S_Dim='\e[2m'
S_Underline='\e[4m'
S_Blink='\e[5m'           # Avoid, not supported.
S_Reverse='\e[7m'         # Invert foreground and background colors.
S_Hidden='\e[8m'          # Useful for passwords.
R_Bold='\e[21m'
R_Dim='\e[22m'
R_Underlined='\e[24m'
R_Blink='\e[25m'
R_Reverse='\e[27m'
R_Hidden='\e[28m'

# These sequences set the foreground color. idx is the color palette
# index number. The names are set to match those in the solarized tables below,
# which seem to be pretty much "the official" color names.
F_Default='\e[39m'
F_Black='\e[30m'            # idx 0
F_Red='\e[31m'              # idx 1
F_Green='\e[32m'            # idx 2
F_Yellow='\e[33m'           # idx 3
F_Blue='\e[34m'             # idx 4
F_Magenta='\e[35m'          # idx 5
F_Cyan='\e[36m'             # idx 6
F_White='\e[37m'            # idx 7
F_BrightBlack='\e[90m'      # idx 8  the "Bright" colors, aka. bold, light or intense
F_BrightRed='\e[91m'        # idx 9
F_BrightGreen='\e[92m'      # idx 10
F_BrightYellow='\e[93m'     # idx 11
F_BrightBlue='\e[94m'       # idx 12
F_BrightMagenta='\e[95m'    # idx 13
F_BrightCyan='\e[96m'       # idx 14
F_BrightWhite='\e[97m'      # idx 15

# Declare an array (0..15) to hold the color escapes. These are easier
# to use when setting up themes such as solarized.
FC=($F_Black $F_Red $F_Green $F_Yellow \
    $F_Blue $F_Magenta $F_Cyan $F_White \
    $F_BrightBlack $F_BrightRed $F_BrightGreen $F_BrightYellow \
    $F_BrightBlue $F_BrightMagenta $F_BrightCyan $F_BrightWhite)

# These sequences set the background color.
B_Default='\e[49m'
B_Black='\e[40m'            # idx 0
B_Red='\e[41m'              # idx 1
B_Green='\e[42m'            # idx 2
B_Yellow='\e[43m'           # idx 3
B_Blue='\e[44m'             # idx 4
B_Magenta='\e[45m'          # idx 5
B_Cyan='\e[46m'             # idx 6
B_White='\e[47m'            # idx 7
B_BrightBlack='\e[100m'     # idx 8
B_BrightRed='\e[101m'       # idx 9
B_BrightGreen='\e[102m'     # idx 10
B_BrightYellow='\e[103m'    # idx 11
B_BrightBlue='\e[104m'      # idx 12
B_BrightMagenta='\e[105m'   # idx 13
B_BrightCyan='\e[106m'      # idx 14
B_BrightWhite='\e[107m'     # idx 15

# Alternative names again.
BC=($B_Black $B_Red $B_Green $B_Yellow \
    $B_Blue $B_Magenta $B_Cyan $B_White \
    $B_BrightBlack $B_BrightRed $B_BrightGreen $B_BrightYellow \
    $B_BrightBlue $B_BrightMagenta $B_BrightCyan $B_BrightWhite)


# For completeness, here are the solarized colors taken from
# http://ethanschoonover.com/solarized

# This set is sorted by palette index for easy reference to the above.
# This corresponds to what you can see in the ConEmu screenshot.

# SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      RGB         HSB         "base16" slot ConEmu.xml
# --------- ------- ---- -------  ----------- ---------- ----------- ----------- ------------- ----------
# base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26 base00        00423607
# red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86 base08        002f32dc
# green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60 base0B        00009985
# yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71 base0A        000089b5
# blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82 base0D        00d28b26
# magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83 base0E        008236d3
# cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63 base0C        0098a12a
# base2     #eee8d5  7/7 white    254 #e4e4e4 92 -00  10 238 232 213  44  11  93 base05        00d5e8ee
# base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21 base03        00362b00
# orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80 base09        00164bcb
# base01    #586e75 10/7 brgreen  240 #585858 45 -07 -07  88 110 117 194  25  46 base01        00756e58
# base00    #657b83 11/7 bryellow 241 #626262 50 -07 -07 101 123 131 195  23  51 base02        00837b65
# base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59 base04        00969483
# violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77 base06        00c4716c
# base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63 base0F        00a1a193
# base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99 base07        00e3f6fd


# And this set is sorted in the same way as Ethan does on the web page

# SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      RGB         HSB         "base16" slot ConEmu.xml
# --------- ------- ---- -------  ----------- ---------- ----------- ----------- ------------- ----------
# base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21 base03        00362b00
# base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26 base00        00423607
# base01    #586e75 10/7 brgreen  240 #585858 45 -07 -07  88 110 117 194  25  46 base01        00756e58
# base00    #657b83 11/7 bryellow 241 #626262 50 -07 -07 101 123 131 195  23  51 base02        00837b65
# base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59 base04        00969483
# base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63 base0F        00a1a193
# base2     #eee8d5  7/7 white    254 #e4e4e4 92 -00  10 238 232 213  44  11  93 base05        00d5e8ee
# base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99 base07        00e3f6fd
# yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71 base0A        000089b5
# orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80 base09        00164bcb
# red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86 base08        002f32dc
# magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83 base0E        008236d3
# violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77 base06        00c4716c
# blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82 base0D        00d28b26
# cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63 base0C        0098a12a
# green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60 base0B        00009985


# #############################################################################################################################
# Boolean functions should return 0 for success, following the same
# convention as unix utilities.

function f_ShowPath()
{
    echo $PATH | tr ':' '\n' | sort
}

# Add a directory to the path if and only if it is not already
# in the path and if it exists.
# Usage: f_AddToPath "~/some/folder"
#        f_AddToPath "/c/Users/Phil/something"
function f_AddToPath()
{
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        PATH="$PATH:$1"
    fi
}

function f_PruneWindowsDirectoriesFromPath()
{
    # Remove all Windows dirs as they appear either in WSL or in Cygwin.
    PATH=`echo $PATH | tr ':' '\n' | grep -v '^/mnt/c/\|^/c/\|^$' | tr '\n' ':'`

    # Remove last :.
    PATH=${PATH::-1}

    if [ $OS == "wsl" ] ; then
        f_AddToPath "/mnt/c/Windows"
        f_AddToPath "/mnt/c/Windows/System32"
    fi

    if [ $OS == "cygwin" ] ; then
        f_AddToPath "/c/Windows"
        f_AddToPath "/c/Windows/System32"
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

    case `uname -r` in
        *Microsoft) OS="wsl" ;;
    esac
}

f_DetermineLinuxDistro()
{
    if [ -f /etc/lsb-release ] ; then
        DISTRO=`cat /etc/lsb-release | grep DISTRIB_ID | cut -d= -f2`
    fi
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

f_ShowInfo()
{
    # Print out various bits of information. This used to be printed when I
    # logged in, but it became tiresome.
    f_DetermineOS
    f_DetermineLinuxDistro
    f_IsRoot
    echo -e "OS=$OS\nTERM=$TERM\nISROOTMSG=$ISROOTMSG"
}

f_AtHome()
{
    # Determine if I am at home or at work.
    case $HOSTNAME in
        RD*) false ;;
        *)   true ;;
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

function f_Install()
{
    # Installs a file using a link. Used in the 00install_dotfiles script.
    local src=$1
    local dest=$2
    f_Relink $src $dest
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

f_Dune()
{
    fortune -a ~/repos/dotfiles/fortunes-dune
}

f_Fortune()
{
    fortune -a ~/repos/dotfiles/fortunes ~/repos/dotfiles/fortunes-dune
}

f_GitSetProxy()
{
    # export http_proxy=http://rdproxy01:800/
    # export https_proxy=http://rdproxy01:800/
    # export http_proxy=http://exproxy03:8080/
    # export https_proxy=http://exproxy03:8080/

    # Edit ~/.gitconfig to add the proxy settings.
    sed -i 's/#proxy =/proxy =/g' ~/.gitconfig
}

f_GitUnsetProxy()
{
    # unset http_proxy
    # unset https_proxy

    # Edit ~/.gitconfig to remove the proxy settings.
    # Assumes a leading space.
    sed -i 's/ proxy =/ #proxy =/g' ~/.gitconfig
}

f_GitPersonalEmail()
{
    # Configures a repository to use my local email irrespective
    # of what the global default is.
    git config user.email Philip.Daniels1971@gmail.com
}

# n.b. To set core.filemode on or off there are two git aliases,
# 'git fmon' and 'git fmoff' will do the trick.

f_GitWorkEmail()
{
    git config user.email Philip.Daniels@Landmark.co.uk
}

f_GitShowConfig()
{
    # Lists critical git configuration which I am always getting wrong
    # when moving from work to home.
    echo -e "${F_Red}Global config:${F_Default}"
    git config --list --global | grep -i 'user.email\|proxy\|fileMode' | sort
    echo -e "\n${F_Red}Local config (takes priority):${F_Default}"
    git config --list --local | grep -i 'user.email\|proxy\|fileMode' | sort
}

function qstart()
{
    # Start a program quietly, dumping all messages from stdout and stderr to /dev/null.
    # This is useful for starting X programs from the command line (especially helpful
    # in WSL).
    "$@" > /dev/null 2>&1 &
}

function qs()
{
    # Same as qstart.
    "$@" > /dev/null 2>&1 &
}


# #############################################################################################################################
# The main event. Note that this has been cut down significantly from what is currently in .bashrc, because KDE neon
# appears to already have much of this stuff in it. No point doing things twice.

f_DetermineOS
f_DetermineLinuxDistro
f_IsRoot

# This environment variable should only be set on Windows, to plink.exe.
# If set on Cywgin or WSL it interferes with git cloning operations.
unset GIT_SSH

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# Add these folders to the path (if they exist).
f_AddToPath "$HOME/bin"
f_AddToPath "$HOME/.cargo/bin"
f_AddToPath "$HOME/.local/bin"

. "$HOME/.cargo/env"


if f_IsCmd "micro"; then
    export VISUAL=micro
    export EDITOR=micro
fi


# Setup nvm (Node Version Manager) to use a folder in my home directory.
if [ -s ~/.nvm/nvm.sh ]; then
    NVM_DIR=~/.nvm
    source ~/.nvm/nvm.sh
fi

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Load bash completion for the dotnet CLI.
_dotnet_bash_complete()
{
  local word=${COMP_WORDS[COMP_CWORD]}

  local completions
  completions="$(dotnet complete --position "${COMP_POINT}" "${COMP_LINE}" 2>/dev/null)"
  if [ $? -ne 0 ]; then
    completions=""
  fi

  COMPREPLY=( $(compgen -W "$completions" -- "$word") )
}

complete -f -F _dotnet_bash_complete dotnet

# #############################################################################################################################

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
alias nclippy='cargo +nightly clippy'
alias sclippy='cargo clippy'

# #############################################################################################################################
# #############################################################################################################################
