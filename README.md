# dotfiles

dotfiles: bash, vim, terminal and misc config for
Linux, Cygwin and MSysGit.

See document INSTALL.md for full installation instructions for a new box.

### To install everything
From an elevated command prompt:

```
@powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup.ps1'))"
```

### To install Cygwin (over an hour)
From an elevated command prompt:

```
@powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/setup_cygwin.ps1'))"
```

