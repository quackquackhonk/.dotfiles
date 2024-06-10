fish_add_path $HOME/bin:/usr/local/bin
fish_add_path $HOME/.local/bin
fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/.sources/sml/bin
fish_add_path $HOME/.sources/smlnj/bin
fish_add_path $HOME/.sources/wabt/bin
fish_add_path $HOME/.sources/v
fish_add_path $HOME/.sources/nvim-macos/bin
fish_add_path /opt/homebrew/bin
fish_add_path /Library/PostgreSQL/16/bin
fish_add_path $HOME/opt/grpc/bin
fish_add_path /home/sahana/.local/share/bob/nvim-bin

if status is-interactive
    # Commands to run in interactive sessions can go here
    
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
    # Custom functions

    function spackon; 
        spack env activate -d spack_env
    end

    function spackoff; 
        unset SPACK_INSTALL_PREFIX
        unset USER_INCLUDE
        unset USER_LIBDIR
        spack env deactivate
    end

    function spackcert
        export CERT_PATH=$(python -c 'import site; print(site.getsitepackages()[0] + "/certifi/cacert.pem")')
        cat ~/cert/ZscalerRootCertificate-2048-SHA256.crt >> $CERT_PATH
    end


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

source /Users/i34866/opt/git/spack/share/spack/setup-env.fish *
