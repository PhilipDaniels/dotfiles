# dotfiles

dotfiles: bash, vim, terminal and misc config for Linux and Cygwin.

See document Public Build Book.odt for full installation instructions for a new box.

### Installation instructions
From an elevated CMD command prompt (not ConEmu because it is part of the install), first install Cygwin so you get Git:

```
REM To install cygwin
@powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup_cygwin.ps1'))"
```

Then clone this repository into ~\repos\dotfiles. Then you can run individual scripts to install or upgrade,
or alternatively just upgrade everything. Then flip to an elevated PowerShell prompt:

```
REM To install/upgrade chocolatey (and a large list of apps) and setup some environment variables
> ./setup_choco.ps1
> choco upgrade all -y

REM To install/upgrade Cygwin after the initial install:
./setup_cygwin.ps1
```

