# Boolean functions should return 0 for success, following the same
# convention as unix utilities.

echo "***** Running dotfiles/.bash_functions"

function ShowPath() {
    echo $PATH | tr ':' '\n' | sort -u
}

pd_apt_update_and_upgrade() 
{
  sudo apt-get update && \
    sudo apt-get -y dist-upgrade && \
    sudo apt-get -y autoremove && \
    sudo apt-get -y autoclean
}

DetermineOS()
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
agent_is_running() {
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

agent_has_keys() {
    ssh-add -l >/dev/null 2>&1
}

agent_load_env() {
    . "$env" >/dev/null
}

agent_start() {
    (umask 077; ssh-agent >"$env")
    . "$env" >/dev/null
}

########################################################################

