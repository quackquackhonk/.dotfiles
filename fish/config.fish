if status is-interactive
    # Commands to run in interactive sessions can go here
    
    alias gs='git status'
    alias ga='git add'
    alias gc='git commit'
    alias gp='git pull'
    alias gP='git push'

    alias cdc='cd /mnt/c/'
    alias cd~='cd /mnt/c/Users/sahan'
    alias cdd='cd /mnt/d/'
    alias cde='cd /mnt/f/'
    alias cdcode='cd ~/code'

    alias fishsrc='source $HOME/.config/fish/config.fish'
    alias fishconf='nvim ~/.config/fish/config.fish'

    alias nvconf='nvim ~/.config/nvim/init.lua'

    alias nv='nvim'
    alias vi='nvim'

    alias ls='exa'
    alias l='exa -lha'

    alias em='emacs -nw'

    alias ta='tmux a'

    alias cgt='cargo nextest run'
    alias cgr='cargo run'
    alias cgb='cargo build'

    export TERM=screen-256color
end

set -g fish_greeting
set -g CMAKE_EXPORT_COMPILE_COMMANDS
set -g DOCKER_DEFAULT_PLATFORM linux/amd64

fish_ssh_agent
ssh-add ~/.ssh/id_ed25519

starship init fish | source

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /opt/homebrew/Caskroom/miniconda/base/bin/conda
    eval /opt/homebrew/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<

export DOCKER_DEFAULT_PLATFORM=linux/amd64
