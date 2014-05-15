" It is surprising we can't use ~/vimfiles/_vimrc but that
" seems to fail in Cygwin vim.
if has("win32unix")
    " We are running cygwin.
    set runtimepath+=/cygdrive/c/Users/Phil/repos/dotfiles/vimfiles
    source /cygdrive/c/Users/Phil/repos/dotfiles/vimfiles/_vimrc
elseif has("unix")
    " We are running under Linux etc.
    set runtimepath+=~/repos/dotfiles/vimfiles
    source ~/repos/dotfiles/vimfiles/_vimrc
elseif has("win32")
    " We are running under Windows.
    set runtimepath+=$UserProfile\repos\dotfiles\vimfiles
    source $UserProfile\repos\dotfiles\vimfiles\_vimrc
endif

