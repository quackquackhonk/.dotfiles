PROMPT='%{$fg[cyan]%}%c%{$reset_color%} $(git_prompt_info)'
PROMPT+="%(?:%{$fg_bold[green]%}λ :%{$fg_bold[red]%}λ )%{$reset_color%}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}(%F{#b48ead%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[red]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"
