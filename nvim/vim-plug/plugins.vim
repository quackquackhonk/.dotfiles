if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  silent autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" All plugins must appear after this line
call plug#begin('~/.config/nvim/autoload/plugged')
" General Plugins

" Utility
Plug 'tpope/vim-surround'	    " Papa tpope, change those surrounding characters
Plug 'jiangmiao/auto-pairs'	    " Auto-pairing / expansion
Plug 'preservim/nerdtree'	    " File browser
Plug 'tpope/vim-commentary'	    " Better commenting support

" Code Completion
" Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" Language support
Plug 'sheerun/vim-polyglot'

" Color highlighting
Plug 'norcalli/nvim-colorizer.lua'

" Themes
" Airline status bar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Nord
Plug 'arcticicestudio/nord-vim'

" All plugins must appear before this line
call plug#end()

autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
