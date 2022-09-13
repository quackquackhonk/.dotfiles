if [ "$USERNAME" = "root" ]; then CARET="#"; else CARET="$"; fi

local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

PROMPT='%{$fg_bold[cyan]%}%n%{$reset_color%} @ %{$fg_bold[cyan]%}%m%{$reset_color%} :: %{${fg[green]}%}%3~ ${reset_color}$(git_prompt_info)%{${reset_color}%}${CARET} '

RPS1="${return_code}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[red]%}‹"
ZSH_THEME_GIT_PROMPT_SUFFIX="› %{$reset_color%}"
