# If you come from bash you might have to change your $PATH.
umask 002

export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH=/opt/homebrew/bin:$PATH
export PATH=/Library/PostgreSQL/16/bin:$PATH
export PATH=$HOME/opt/grpc/bin:$PATH

export DOCKER_DEFAULT_PLATFORM=linux/amd64
export HOMEBREW_NO_AUTO_UPDATE=1

export CMAKE_COLOR_DIAGNOSTICS=ON
export CMAKE_EXPORT_COMPILE_COMMANDS=1

unsetopt PROMPT_SP

plugins=(git ssh-agent)

export EDITOR="emacsclient -nw -a=''"

# User configuration

# aliases
alias zshconf="$EDITOR ~/.zshrc && source ~/.zshrc"
alias zshsrc="source ~/.zshrc"

alias ls='eza'
alias l='eza -lah'
alias b='bat'
alias cd='z'

alias t='tmux'

alias em='emacsclient -nw'
alias emd='emacs --debug-init --fg-daemon'
alias emkill='emacsclient -e "(kill-emacs)"'

# work
alias cirrus='ssh -i ~/.ssh/stankala_id.key stankala@cirrus.veriskweather.net -t "zsh"'

export BAT_THEME="Catppuccin Mocha"
export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"


# ssh agent
# eval "$(ssh-agent -s)"
# eval "$(ssh-add ~/.ssh/id_ed25519)"source <(fzf --zsh)

eval "$(zoxide init zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(starship init zsh)"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
