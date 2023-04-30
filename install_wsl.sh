#!/bin/bash
# cp -r $HOME/dotfiles/nvim /mnt/c/Users/sahan/AppData/Local/
mkdir /mnt/c/Users/sahan/AppData/Roaming/alacritty
cp $HOME/.dotfiles/alacritty/wsl.yml  /mnt/c/Users/sahan/AppData/Roaming/alacritty/alacritty.yml
cp $HOME/.dotfiles/emacs/init.el /mnt/c/msys64/home/sahan/.emacs.d/init.el
cp $HOME/.dotfiles/emacs/config.org /mnt/c/msys64/home/sahan/.emacs.d/config.org
