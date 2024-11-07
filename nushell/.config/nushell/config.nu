# version = "0.96.1"
# The default config record. This is where much of your global configuration is setup.
$env.config = {
    show_banner: false

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
    buffer_editor: emacsclient

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
alias g = git
alias gu = gitui
alias gd = git diff
alias gs = git status
alias ga = git add
alias gc = git commit
alias gp = git pull
alias gP = git push

alias l = ls -las
alias b = bat

# Edit the given file using 'emacsclient'
# If the emacs daemon is not running, starts one and attach to it.
def edit [...args] {
    emacsclient -a="" ...$args
}
alias e = edit
alias emkill = emacsclient -e "(kill-emacs)"

# Open my init.el file
def "config emacs" [] {
    edit ~/.emacs.d/init.el
}

pokemon-colorscripts --no-title -r 1-8

# CLI tool integrations
use ~/.cache/starship/init.nu
source ~/.zoxide.nu

use ($nu.default-config-dir | path join mise.nu)