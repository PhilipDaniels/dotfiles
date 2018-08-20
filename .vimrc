" Necessary folders are created in the dotfiles installation script
" rather than every time vim starts.

" =============== Start of Vundle Setup ===============
set nocompatible    " Do not try to be backward compatible. [Required by Vundle.]
filetype off        " [Required by Vundle.]

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
call vundle#end()            " required
filetype plugin indent on    " required

" ========== End of Vundle Setup - all other config comes after this ==========


set noswapfile      " Turn off creation of .swp files.
set nobackup        " Turn off creation of ~ backup files.
set number          " Turn on line numbers.
set laststatus=2    " Turn on the status line.
set tabstop=4       " Make tab characters appear 4 spaces wide.
set shiftwidth=4    " Indents will have a width of 4.
set expandtab       " Make the tab key insert spaces instead of a tab.
set autoindent      " Copy indentation from current line when starting a new line.

" Turn on creation of persistent undo files.
set undodir=~/.vim/undo
set undofile

" Q will reply a macro stored in the register q.
" Use qq to record such a macro, and q to terminate recording.
" 5Q will run it 5 times.
:nnoremap Q @q

