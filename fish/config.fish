if set -q ZELLIJ
else
    zellij
end

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

    alias em='emacs -nw'

    alias ta='tmux a'
    alias z='zellij'

    alias cgt='cargo nextest run'
    alias cgr='cargo run'
    alias cgb='cargo build'
<<<<<<< HEAD

=======
    
>>>>>>> 976ad03 (WOOOO)
    export TERM=screen-256color

    function zr
      command zellij run --name "$argv" -- fish -c "$argv"
    end
    function zrf
      command zellij run --name "$argv" --floating -- fish -c "$argv"
    end
    function ze
      command zellij edit $argv
    end
    function zef
      command zellij edit --floating $argv
    end
end

set -g fish_greeting

source $XDG_CONFIG_HOME/fish/completions/zellij.fish

fish_ssh_agent

starship init fish | source
eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
