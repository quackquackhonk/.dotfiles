" set leader key
let g:mapleader = "\<Space>"

" -- General Settings --
" this is a change
set nocompatible            " VimWiki
set number		    " Shows line numbers
set relativenumber          " Enable relative line numbering
set ruler                   " Display cursor position
set hidden                  " Required to keep multiple buffers open
" set colorcolumn=80	    " Column guide
set nowrap                  " Disable line wrapping
set modifiable              " Modifiable on
set encoding=utf-8          " Use UTF-8 encoding
set fileencoding=utf-8      " Use UTF-8 file encoding
set iskeyword+=-            " Treats dash-separated words as a word text object
set mouse=a                 " Enable mouse
if has('termguicolors')
    set termguicolors       " Enables true colors
endif
set pumheight=10            " Decrease Pop-Up Menu height
set splitbelow              " Horizontal splits default to opening below
set splitright              " Vertical splits default to opening to the right
set clipboard=unnamedplus   " Copy/Paste between nvim and everything else
set ignorecase              " Searching is case insensitive
set smartcase               " Case sensitive searching only when search query contains caps
filetype plugin indent on   " You know what this does...

" -- Tab settings --
set expandtab               " No expansion of tabs
set tabstop=4
set softtabstop=4           " :help tabstop told me to
set shiftwidth=4            " Change number of space characters inserted for indentation
set smarttab                " Smarter tabs
set autoindent              " Auto indenting
