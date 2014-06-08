Setting up Solarized
====================
Solarized homepage: http://ethanschoonover.com/solarized
          Github  : https://github.com/altercation/vim-colors-solarized

I consider the base standard for what solarized should look like to be
the screenshots at the website, and how it looks in gVim. gVim always
gets the colors right because it supports 16million colors. The aim is
to get the terminals to look like that.

Setup of .bashrc
----------------
It is considered a bad idea to override your $TERM variable using

  export TERM=xterm-256color

or similar. This is because you are essentially lying about what you terminal
is. It is better to use a terminal that can be properly configured, i.e.
one that supports 256 colors is ideal.


Terminal Color Support
----------------------
All modern terminals should support 256 colors, especially all X-based
terminals. 

The Linux console is 8/16? colors only.

MSysGit and Cygwin can be started under Windows by bat files which typically
run bash shells. These appear to be based on the same underlying console
which only supports 8/16 colors. MSysGit also has rxvt (also 8 colors), and
Cygwin has mintty (256 color support).

MSysGit does not appear to support sufficient colors, but it works perfectly
when run inside ConEmu (always set ConEmu color scheme to solarized).

Use the 256colors.pl script and colors.sh script to gauge support. You can
also do 
  prompt> tput colors
but Cygwin and MSysGit do not have this. MSYS2 does.


Linux console (OS = linux, TERM = linux, COLORS = 8/16?)
--------------------------------------------------------
n.b. The Linux console only supports 8 or possibly 16 colors.
Use this: https://github.com/EvanPurkhiser/linux-vt-setcolors
to configure the color palette.

My 00install.sh script compiles and installs this program if
it detects the OS as "Linux".

[ ] In Hyper-V, shell ls and prompt colors are washed out (everything is grey).
[ ] In Hyper-V, console vim is mainly ok, but the status line is hard to see.

Linux XTerm
-----------
TODO.

Linux Terminator
----------------
TODO.

MSysGit via git-bash.bat (OS = msys, TERM = , COLORS = 8/16?)
-------------------------------------------------------------

MSysGit under ConEmu (OS = msys, TERM = cygwin, COLORS = 8/16?)
-----------------------------------------------
ConEmu task: "%UserProfile%\OtherApps\PortableGit\bin\sh" --login -i

Set the ConEmu color scheme to solarized using the setup dialog and ensure
that 256 color support is enabled by ticking "Inject ConEmuHk"  and
"ANSI X3.64 / xterm 256 colors" on the features page.

[ ] The vim that ships with MSysGit does not appear to work correctly
    (everything looks grey) so alias "vim" to the native Windows vim.
    Syntax and colors then look very similar to gVim.

[ ] SSH to Debian: 'tput colors' reports only 8 colors.
    Directory listings are similar to MSysGit-local, but vim looks
    funny and reports only 8 color support. This is probably
    because the terminal is the same as the cygwin one, and it really
    only does support 8 colors.

MSysGit via Cygwin's mintty
---------------------------
This does not work. MSysGit is unable to use mintty.
https://github.com/msysgit/git/issues/60

Cygwin via Cygwin.bat (OS = cygwin, TERM = cygwin, COLORS = 8/16)
-----------------------------------------------------------------
This basically runs: C:\cygwin64\bin\bash --login -i
Neither of the color scripts looks good.
 
Cygwin via mintty (OS = cygwin, TERM = various, COLORS = 256)
-------------------------------------------------------------
Mintty home page: https://code.google.com/p/mintty/
See the wiki for tips on starting up with ssh or from DOS batch files
and configuring colors.

Color scripts look good. You need to customize mintty via a ~/.minttyrc
to get solarized, see: https://github.com/mavnn/mintty-colors-solarized

Various ways of starting it up (if mintty is not on your path
use a full path to the exe).

1) mintty.exe -     (note the dash)
2) mintty.exe -c ~/.minttyrc.solarized.dark - 
3) mintty.exe mc.exe 
 
1) Starts a login shell using the default ~/.minttyrc.
2) Another login shell, but specify which config file to use
3) Start Midnight Commander with default colors

[ ] Windows native Vim does not work at all, the cursor keys
    are borked and/or the window never displays!
[ ] However the Cygwin vim now works well. Best to set 
    "Term=xterm-256color" in .minttyrc.


Cywgin under ConEmu, via bash (OS = cygwin, TERM = cygwin)
----------------------------------------------------------
ConEmu task: C:\cygwin64\bin\bash --login -i
This is the same as starting Cygwin by clicking the "Cygwin.bat"
file. Neither of the color scripts looks good.

Cygwin under Conemu, via mintty (OS = cygwin, TERM = xterm, COLORS = 256)
-------------------------------------------------------------------------
ConEmu task: C:\cygwin64\bin\mintty.exe -
None of ConEmu's solarized theme is picked up.
All comments under "Cygwin via mintty" apply.


Setup of Vim
------------
* gVim - it just works.

* console vim - the method is to first set your console's colorscheme
to solarized. For this, you will need a terminal that supports more
than the standard number of colors. Then the vimrc needs some tweaking
because the status line etc. are not as visible as I like.

* Vim as a ConEmu task

mc (Midnight Commander)
-----------------------
TODO

tmux
----
TODO

htop
----
TODO

