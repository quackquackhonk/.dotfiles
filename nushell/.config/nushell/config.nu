# version = "0.96.1"

# The default config record. This is where much of your global configuration is setup.
$env.config = {
    ls: {
        use_ls_colors: true # use the LS_COLORS environment variable to colorize output
        clickable_links: false # enable or disable clickable links. Your terminal has to support links.
    }

    rm: {
        always_trash: false # always act as if -t was given. Can be overridden with -p
    }

    table: {
        mode: rounded
        index_mode: auto # "always" show indexes, "never" show indexes, "auto" = show indexes when a table has "index" column
    }

    cursor_shape: {
        emacs: block # block, underscore, line, blink_block, blink_underscore, blink_line, inherit to skip setting cursor shape (line is the default)
    }
    edit_mode: emacs

    keybindings: [
        {
            name: reload_config
                modifier: none
                keycode: f5
                mode: [emacs vi_normal vi_insert]
                event: {
                    send: executehostcommand,
                    cmd: $"source '($nu.env-path)';source '($nu.config-path)'"
                }
        }
    ]
}

# ALIASES
## Git
alias g = git
alias gu = gitui
alias gd = git diff
alias gs = git status
alias ga = git add
alias gc = git commit
alias gp = git pull
alias gP = git push

alias hyprconf = nvim ~/.config/hypr/hyprland.conf
alias nixconf = nvim ~/dotfiles/nixos/flake.nix 
alias nvconf = nvim ~/.config/nvim/init.lua 
alias nuconf = nvim ~/.config/nushell/config.nu 

alias cirrus = ssh -i ~/.ssh/stankala_id.key stankala@cirrus.veriskweather.net -t "zsh"

alias nv = nvim
alias vi = nvim

alias em = emacs -nw
alias ec = emacsclient -nw

alias t = zellij
alias b = bat

alias cgt = cargo nextest run
alias cgr = cargo run
alias cgb = cargo build

use ~/.cache/starship/init.nu
source ~/.zoxide.nu
