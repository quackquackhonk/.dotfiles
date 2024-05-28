local wezterm = require("wezterm")

local config = wezterm.config_builder()

-- set environment variable for correct colors
config.set_environment_variables = {
	TERM = "xterm-256color",
}

-- fonts and color
config.color_scheme = "Catppuccin Mocha"
config.colors = {
	background = "#181825",
}
config.font = wezterm.font("MapleMono Nerd Font")
config.bold_brightens_ansi_colors = true
config.font_size = 12
config.freetype_load_target = "Light"

config.default_cursor_style = "BlinkingBlock"

-- window settings
config.use_fancy_tab_bar = false
config.enable_tab_bar = false

return config
