local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

PROMPT='%{$fg_bold[magenta]%}%n%{$reset_color%} @ %{$fg_bold[magenta]%}%m%{$reset_color%} :: %{${fg[green]}%}%3~ ${reset_color}$(git_prompt_info)%{${reset_color}%}| '

RPS1="${return_code}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}‹"
ZSH_THEME_GIT_PROMPT_SUFFIX="› %{$reset_color%}"
