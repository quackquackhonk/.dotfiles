# Nushell Environment Config File

use std *

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# $env.PATH = ($env.PATH | split row (char esep) | prepend '/some/path')
# An alternate way to add entries to $env.PATH is to use the custom command `path add`
 which is built into the nushell stdlib:
# $env.PATH = ($env.PATH | split row (char esep))
path add /opt/homebrew/bin
path add ($env.HOME | path join ".local/bin")
path add ($env.HOME | path join ".cargo/bin")
path add ($env.HOME | path join "bin")
path add ($env.HOME | path join ".sources/sml/bin")
path add ($env.HOME | path join ".sources/smlnj/bin")
path add ($env.HOME | path join ".sources/wabt/bin")
path add ($env.HOME | path join ".sources/nvim-macos-arm64/bin")
$env.PATH = ($env.PATH | uniq)

$env.TERM = "xterm-256color"
$env.COLORTERM = "truecolor"

$env.EDITOR = "emacs"
$env.VISUAL = "emacs"

# starship
mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu

zoxide init nushell | save -f ~/.zoxide.nu
