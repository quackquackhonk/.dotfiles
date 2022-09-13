if status is-interactive
    # Commands to run in interactive sessions can go here
    #
    alias gs='git status'
    alias cdc='cd /mnt/c/'
    alias cd~='cd /mnt/c/Users/sahan'
    alias cdd='cd /mnt/d/'
    alias cde='cd /mnt/f/'
    alias cdcode='cd ~/code'
    alias fish_reload='source $HOME/.config/fish/config.fish'
    alias nv='nvim'
    alias vi='nvim'
    alias em='emacs -nw'
    alias ta='tmux a'
    alias cgt='cargo nextest run'
    alias cgr='cargo run'
    alias cgb='cargo build'
    export TERM=screen-256color
end
