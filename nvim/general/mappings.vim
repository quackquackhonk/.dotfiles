" Better nav for omnicomplete
inoremap <expr> <c-j> ("\<C-n>")
inoremap <expr> <c-k> ("\<C-p>")

" Use shift + alt + hjkl to resize windows
nnoremap <S-M-j>    :resize -2<CR>
nnoremap <S-M-k>    :resize +2<CR>
nnoremap <S-M-h>    :vertical resize -2<CR>
nnoremap <S-M-l>    :vertical resize +2<CR>

" tab in general mode will move to tab
nnoremap <TAB> :bnext<CR>
" shift + tab will go back
nnoremap <S-TAB> :bprevious<CR>

" Alternate way to save
nnoremap <C-s> :w<CR>
" Alternate way to quit
nnoremap <C-Q> :wq!<CR>
" Alternate way to force quit
nnoremap <C-M-Q> :q!<CR>
" Use control-c instead of escape
nnoremap <C-c> <Esc>
" <TAB>: completion.
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

" Better tabbing
vnoremap < <gv
vnoremap > >gv

" Better window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Move lines use alt + jk
nnoremap <M-j> :m +1<CR>
nnoremap <M-k> :m -2<CR>
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv
