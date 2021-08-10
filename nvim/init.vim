"always source these first
runtime! vim-plug/plugins.vim
runtime! general/settings.vim

" Key mapping
runtime! general/mappings.vim

" Plugin configuration
runtime! config/*.vim
" luafile $HOME/.config/nvim/config/plug-colorizer.lua

" Should always be sourced last
runtime! themes/vanity.vim
