# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

umask 002

export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH=/Library/PostgreSQL/16/bin:$PATH
export PATH=$HOME/opt/grpc/bin:$PATH
export PATH=/Library/Frameworks/Python.framework/Versions/3.11/bin:$PATH

# export PKG_CONFIG_PATH="/opt/homebrew/lib/pkgconfig"

export DOCKER_DEFAULT_PLATFORM=linux/amd64
export DOCKER_BUILDKIT=0
export HOMEBREW_NO_AUTO_UPDATE=1

export CMAKE_COLOR_DIAGNOSTICS=ON
export CMAKE_EXPORT_COMPILE_COMMANDS=1
export CMAKE_OSX_SYSROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk

unsetopt PROMPT_SP

export TERM=xterm-256color
export COLORTERM=truecolor

# homebrew stuff
# source ~/.zprofile
alias brew=/opt/homebrew/bin/brew
function brewlink() {
    ln -s /opt/homebrew/bin/${1} ~/.local/bin/${1}
}
function brewlinksys() {
    sudo ln -s /opt/homebrew/bin/${1} /usr/local/bin/${1}
}

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

alias ls='eza'
alias l='eza -lah'

# aliases
alias zshsrc="source ~/.zshrc"

alias emd='emacs --daemon'
alias emdebug='emacs --debug-init --fg-daemon'
alias emkill='emacsclient -e "(kill-emacs)"'
alias ec='emacsclient -nw -c -a=""'

# work
alias cirrus='ssh -i ~/.ssh/stankala_id.key stankala@cirrus.veriskweather.net -t "zsh"'

alias nonprod-login='okta-aws-cli web --profile nonprod  --expiry-aws-variables --cache-access-token --write-aws-credentials'
alias nonprod-east-login='okta-aws-cli web --profile nonprod-east  --expiry-aws-variables --cache-access-token --write-aws-credentials'
alias production-login='okta-aws-cli web --profile production --expiry-aws-variables --cache-access-token --write-aws-credentials'

alias awsnp='aws --profile nonprod'
alias awsprod='aws --profile production'

if [[ $HOST == "LVV3TW207K" ]]; then
    alias docker='podman'
fi
alias nonprod='eval "$(aws configure export-credentials --profile nonprod --format env)" && aws ecr get-login-password --region us-west-2 | docker login --username AWS --password-stdin 589310964831.dkr.ecr.us-west-2.amazonaws.com && export AWS_PROFILE="nonprod"'
alias nonprod-east='eval "$(aws configure export-credentials --profile nonprod-east --format env)" && aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 589310964831.dkr.ecr.us-east-1.amazonaws.com && export AWS_PROFILE="nonprod-east"'

alias production='eval "$(aws configure export-credentials --profile production --format env)"'

alias spackon='spack env activate -d spack_env/.'
alias spackoff='spack env deactivate'
alias python='python3.11'

function spackcert () {
    SPACK_CERT_PATH=$(python -c 'import site; print(site.getsitepackages()[0] + "/certifi/cacert.pem")')
    cat ~/cert/ZscalerRootCertificate-2048-SHA256.crt >> $SPACK_CERT_PATH
    export SSL_CERT_FILE=$SPACK_CERT_PATH
}

export CERT_PATH=$(python3 -c 'import site; print(site.getsitepackages()[0] + "/certifi/cacert.pem")')
export SSL_CERT_FILE=$CERT_PATH

export BAT_THEME="Catppuccin Mocha"
export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

# integration
eval "$(zoxide init zsh)"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval "$(starship init zsh)"
eval "$(direnv hook zsh)"

. ~/opt/git/spack/share/spack/setup-env.sh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/storage/stankala/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/storage/stankala/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/storage/stankala/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/storage/stankala/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
