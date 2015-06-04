param([Int32]$arch=64)

# Main environment.
iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup_env.ps1'))

# Install Cygwin.
iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup_cygwin.ps1'))

# Install chocolatey apps.
iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup_choco.ps1'))


