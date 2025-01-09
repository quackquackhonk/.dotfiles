fish_add_path /bin:/usr/local/bin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.sources/sml/bin:$HOME/.sources/smlnj/bin:$HOME/.sources/wabt/bin:$HOME/.sources/v:$HOME/.sources/nvim-macos-arm64/bin

if status is-login
    if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        exec Hyprland
    end
end

# export CERT_PATH=$(python -c 'import site; print(site.getsitepackages()[0] + "/certifi/cacert.pem")')
# export SSL_CERT_FILE=$CERT_PATH

if status is-interactive
    export TERM=xterm-256color
    export EDITOR='emacs -nw'
    export HOMEBREW_NO_AUTO_UPDATE=1

    export FZF_DEFAULT_OPTS=" \
    --color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
    --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
    --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"
    export BAT_THEME="Catppuccin Mocha"
    export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

    alias ec='emacsclient'
    alias emd='emacs --daemon'
    alias killemacs='emacsclient -e "(kill-emacs)"'

    alias g='git'
    alias gu='gitui'
    alias gd='git diff'
    alias gs='git status'
    alias ga='git add'
    alias gc='git commit'
    alias gp='git pull'
    alias gP='git push'

    alias fishsrc='source $HOME/.config/fish/config.fish'
    alias hyprconf='$EDITOR ~/.config/hypr/hyprland.conf'
    alias fishconf='$EDITOR ~/.config/fish/config.fish && source $HOME/.config/fish/config.fish'
    alias nvconf='$EDITOR ~/.config/nvim/init.lua'
    alias nixconf='$EDITOR ~/dotfiles/nixos/flake.nix'

    alias cirrus='ssh -i ~/.ssh/stankala_id.key stankala@cirrus.veriskweather.net -t "zsh"'

    alias nv='nvim'

    alias l='ls -last'
    alias t='zellij'
    alias b='bat'

    alias cgt='cargo nextest run'
    alias cgr='cargo run'
    alias cgb='cargo build'
end

set -g fish_greeting
set -g CMAKE_EXPORT_COMPILE_COMMANDS

starship init fish | source
fzf --fish | source

zoxide init fish | source

 >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /opt/homebrew/Caskroom/miniconda/base/bin/conda
    eval /opt/homebrew/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
else
    if test -f "/opt/homebrew/Caskroom/miniconda/base/etc/fish/conf.d/conda.fish"
        . "/opt/homebrew/Caskroom/miniconda/base/etc/fish/conf.d/conda.fish"
    else
        set -x PATH "/opt/homebrew/Caskroom/miniconda/base/bin" $PATH
    end
end
# <<< conda initialize <<<
