fish_add_path /opt/homebrew/bin
fish_add_path $HOME/bin:/usr/local/bin
fish_add_path $HOME/.local/bin
fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/.sources/sml/bin
fish_add_path $HOME/.sources/smlnj/bin
fish_add_path $HOME/.sources/wabt/bin
fish_add_path $HOME/.sources/v
fish_add_path $HOME/.sources/nvim-macos-arm64/bin

if status is-login
    if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        exec Hyprland
    end
end

if status is-interactive
    export TERM=xterm-256color
    export EDITOR=nvim
    export HOMEBREW_NO_AUTO_UPDATE=1

    export FZF_DEFAULT_OPTS=" \
    --color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
    --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
    --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"
    export BAT_THEME="Catppuccin Mocha"
    export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc


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
    alias vi='nvim'

    alias ls='eza'
    alias l='eza -lha'
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

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /opt/homebrew/Caskroom/miniconda/base/bin/conda
    eval /opt/homebrew/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<

# Spack
if [ "$hostname" = "LVV3TW207K" ]

    source /Users/i34866/opt/git/spack/share/spack/setup-env.fish

    function spackon
        spack env activate -d spack_env/
    end

    function spackoff
        set -e SPACK_INSTALL_PREFIX
        set -e USER_INCLUDE
        set -e USER_LIBDIR
        spack env deactivate
    end

    function spackcert
        export CERT_PATH=$(python -c 'import site; print(site.getsitepackages()[0] + "/certifi/cacert.pem")')
        cat ~/cert/ZscalerRootCertificate-2048-SHA256.crt >> $CERT_PATH
    end

    # Okta AWS utils
    ## Nonprod login / cli wrapper
    # authenticate 
    alias nonprod-login='okta-aws-cli web --profile nonprod --session-duration 28800 --expiry-aws-variables --cache-access-token --write-aws-credentials'
    alias production-login='okta-aws-cli web --profile production --expiry-aws-variables --cache-access-token --write-aws-credentials'

    alias awsnp='aws --profile nonprod'
    alias awsprod='aws --profile production'

    function nonprod
        # set the profile variable
        export AWS_PROFILE="nonprod"
    end

    function production
        export AWS_PROFILE="production"
    end
end

zoxide init fish | source
/Users/i34866/.local/bin/mise activate fish | source
