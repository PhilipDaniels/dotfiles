# dotfiles

dotfiles: bash, vim, terminal and misc config for
Linux, Cygwin and MSysGit.

See document INSTALL.md for full installation instructions for a new box.

### Installation instructions
From an elevated command prompt:

```
REM To install everything
@powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup.ps1'))"

REM Just to setup the "main" env (needed if not installing everything)
@powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup_env.ps1'))"

REM To install cygwin
@powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup_cygwin.ps1'))"

REM To install chocolatey (and a large list of apps)
@powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup_choco.ps1'))"
```

### Upgrade instructions
```
choco upgrade all -y
```

or just run this script again to upgrade everything.
