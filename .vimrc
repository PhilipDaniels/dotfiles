" It is surprising we can't use ~/vimfiles/_vimrc but that
" seems to fail in Cygwin vim.
if has("win32unix")
    " We are running cygwin.
    set runtimepath+=/cygdrive/c/PublicHome/dotfiles/vimfiles
    source /cygdrive/c/PublicHome/dotfiles/vimfiles/_vimrc
elseif has("unix")
    " We are running under Linux etc.
    set runtimepath+=~/repos/github/dotfiles/vimfiles
    source ~/repos/github/dotfiles/vimfiles/_vimrc
elseif has("win32")
    " We are running under Windows.
    set runtimepath+=C:\PublicHome\dotfiles\vimfiles
    source C:\PublicHome\dotfiles\vimfiles\_vimrc
endif

