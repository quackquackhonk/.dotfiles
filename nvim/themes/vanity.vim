" Vanity Settings
" Color schemes, background colors, anything with no concrete functionality

" Initializations
syntax enable			" Enable syntax highlighting
set t_Co=255			" Support for 256 colors
set background=dark		" Tell nvim what background color to use
set cursorline			" Highlights the current line

" Color schemes
" Set nvim colorscheme
" Also sets airline theme
let color_path = expand('~/.config/nvim/themes/color.vim')
if filereadable(color_path)
  exec 'source' color_path
else
  " Loads default color scheme
  colorscheme nord
  let g:airline_theme='nord'
endif  

" Transparent bg using color schemes 
" hi! Normal ctermbg=NONE guibg=NONE
" hi! NonText ctermbg=NONE guibg=NONE guifg=NONE ctermfg=NONE
" hi! SignColumn ctermbg=NONE guibg=NONE guifg=NONE ctermfg=NONE
" hi! CursorLineNR ctermbg=NONE guibg=NONE
" hi! LineNr ctermbg=NONE guibg=NONE
