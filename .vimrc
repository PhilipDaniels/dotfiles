" Necessary folders are created in the dotfiles installation script
" rather than every time vim starts.

" =============== Start of Vundle Setup ===============
set nocompatible    " Do not try to be backward compatible. [Required by Vundle.]
filetype off        " [Required by Vundle.]

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'   " Let Vundle manage Vundle, required.
Plugin 'iCyMind/NeoSolarized'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'itchyny/lightline.vim'
Plugin 'scrooloose/nerdtree'
call vundle#end()               " Required.
filetype plugin indent on       " Required.

" ========== End of Vundle Setup - all other config comes after this ==========

set noswapfile      " Turn off creation of .swp files.
set nobackup        " Turn off creation of ~ backup files.
set number          " Turn on line numbers.
set laststatus=2    " Turn on the status line.
set tabstop=4       " Make tab characters appear 4 spaces wide.
set shiftwidth=4    " Indents will have a width of 4.
set expandtab       " Make the tab key insert spaces instead of a tab.
set autoindent      " Copy indentation from current line when starting a new line.
set noshowmode      " Turn off -- INSERT -- appearing at the bottom. Not necessary since we have lightline.
set backspace=indent,eol,start  " In insert mode, allow deletion of the end of line.

" Turn on creation of persistent undo files.
set undodir=~/.vim/undo
set undofile

" Q will reply a macro stored in the register q.
" Use qq to record such a macro, and q to terminate recording.
" 5Q will run it 5 times.
:nnoremap Q @q

syntax on           " Turn on syntax highlighting.
if has('termguicolors')
    set termguicolors
endif
set background=dark
colorscheme NeoSolarized
let g:lightline = { 'colorscheme': 'solarized' }

" Make Ctrl-P invoke the Ctrl-P package (duh).
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_show_hidden = 1

" Make Vim and Ctrl-P ignore Rust build artifacts.
set wildignore+=*/target/debug/*,*/target/release/*

