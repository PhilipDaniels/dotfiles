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
    "$@" > /dev/null 2>&1 &
}
