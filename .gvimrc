if has("win32unix")
    " We are running cygwin.
    source /cygdrive/c/PublicHome/dotfiles/vimfiles/_gvimrc
elseif has("unix")
    " We are running under Linux etc.
    source ~/repos/github/dotfiles/vimfiles/_gvimrc
elseif has("win32")
    " We are running under Windows.
    source C:\PublicHome\dotfiles\vimfiles\_gvimrc
endif

