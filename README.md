dotfiles
========

dotfiles + Debian/Linux install scripts

Replace or copy the .files into your home dir.
The script should work on Linux, MSysGit and Cygwin.

The setup folder contains scripts to run after a Debian NetInstall
to create a usable machine.


Setting up Solarized
====================
Solarized homepage: http://ethanschoonover.com/solarized
I consider the base standard for what solarized should look like to be
the screenshots at the website, and how it looks in gVim. gVim always
gets the colors right because it supports 16million colors. The aim is
to get the terminals to look like that.

Setup of .bashrc
----------------
It is considered a bad idea to specify what your terminal is using
  export TERM=xterm-256color
or similar. This is because you are eseentially lying about what you terminal
is. It is better to use a terminal that can be properly configured.

Setup of Linux terminals
------------------------
* Linux console. (OS = linux, TERM = linux)
n.b. The Linux console only supports 16 colors. I have a function in
.bash_functions which prints escape codes to the terminal. This sets the
palette to be solarized.
  [ ] In a direct Hyper-V connection, shell ls and prompt colors are
      washed out (everything is grey).
  [ ] In a direct Hyper-V connection, console vim is mainly ok, but the status
      line is hard to see.

* Xterm. TODO.

* Terminator. TODO.


Setup of Windows terminals
--------------------------
* ConEmu. Set the color scheme to solarized using the setup dialog.

* mintty. This is the terminal emulator used by Cygwin and MSysGit
(though it may be reported as "cygwin" or "xterm"). It can be
started directly:
  C:\cygwin64\bin\mintty.exe -
and will read config from ~/.minttyrc

https://github.com/mavnn/mintty-colors-solarized

* It is possible to run mintty under Conemu by setting up a task
  this starts a Cywgin shell. None of ConEmu's solarized theme or
  fonts is picked up though.

Setup of terminals under SSH
----------------------------


Setup of Vim
------------
* gVim - it just works.

* console vim - the method is to first set your console's colorscheme
to solarized. For this, you will need a terminal that supports more
than the standard number of colors. Then the vimrc needs some tweaking
because the status line etc. are not as visible as I like.


ConEmu
MSysGit
mc
Cygwin
Linux VT (on Debian)
XTerm
mintty (shipped with Cygwin)
tmux - doesn't work in Cygwin yet.

