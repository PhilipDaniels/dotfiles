param([Int32]$arch=64)

# setup.ps1 - Main installation script. Now attempt to automate
# all steps documented in INSTALL.md.


# Create the environment variables that various programs need.
[Environment]::SetEnvironmentVariable("CYGROOT", $cygDir, "User")
[Environment]::SetEnvironmentVariable("FARROOT", "C:\Public\Apps\Far30b4242.x64.20150117", "User")
[Environment]::SetEnvironmentVariable("PORTABLEAPPSROOT", "C:\PortableApps", "User")
[Environment]::SetEnvironmentVariable("PUBLICROOT", "C:\Public", "User")
Write-Host "Environment variables created"


# Install Chocolatey
Write-Host "Installing Chocolatey..."
iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))
Write-Host "Chocolatey should be installed."


# Install Cygwin.
iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup_cygwin.ps1'))

# Install fonts.


# Create ~/repos


# Download repos


