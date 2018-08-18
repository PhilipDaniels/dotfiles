" Ensure ~/.vim and relevant subfolders are created.
if !isdirectory($HOME."/.vim")
    call mkdir($HOME."/.vim", "", 0770)
endif
if !isdirectory($HOME."/.vim/undo")
    call mkdir($HOME."/.vim/undo", "", 0700)
endif
if !isdirectory($HOME."/.vim/swap")
    call mkdir($HOME."/.vim/swap", "", 0700)
endif
if !isdirectory($HOME."/.vim/backup")
    call mkdir($HOME."/.vim/backup", "", 0700)
endif

" Turn off creation of .swp files.
set noswapfile

" Turn off creation of ~ backup files.
set nobackup

" Turn on creation of persistent undo files.
set undodir=~/.vim/undo
set undofile

