fish_add_path $HOME/bin:/usr/local/bin
fish_add_path $HOME/.local/bin
fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/.sources/sml/bin
fish_add_path $HOME/.sources/smlnj/bin
fish_add_path $HOME/.sources/wabt/bin
fish_add_path $HOME/.sources/v
fish_add_path /home/sahana/.local/share/bob/nvim-bin

if status is-login
    if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        exec Hyprland
    end
end

if status is-interactive
    # Commands to run in interactive sessions can go here
    alias sudo="doas"
    
    alias gs='git status'
    alias ga='git add'
    alias gc='git commit'
    alias gp='git pull'
    alias gP='git push'

    alias fishsrc='source $HOME/.config/fish/config.fish'
    alias fishconf='nvim ~/.config/fish/config.fish && source $HOME/.config/fish/config.fish'

    alias nvconf='nvim ~/.config/nvim/init.lua'

    alias nv='nvim'
    alias vi='nvim'

    alias ls='eza'
    alias l='eza -lha'
    alias t='zellij'
    alias b='bat'

    alias cgt='cargo nextest run'
    alias cgr='cargo run'
    alias cgb='cargo build'

    alias awsnonprod='saml2aws login -a nonprod && eval $(saml2aws script -a nonprod)'
    export TERM=xterm-256color
end

set -g fish_greeting
set -g CMAKE_EXPORT_COMPILE_COMMANDS
set -g DOCKER_DEFAULT_PLATFORM linux/amd64
export DOCKER_DEFAULT_PLATFORM=linux/amd64

starship init fish | source
fzf --fish | source

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /opt/homebrew/Caskroom/miniconda/base/bin/conda
    eval /opt/homebrew/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<

zoxide init fish | source
