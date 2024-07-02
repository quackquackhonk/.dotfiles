local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.hyperlink_rules = wezterm.default_hyperlink_rules()

-- set environment variable for correct colors
config.set_environment_variables = {
	TERM = "xterm-256color",
}

-- fonts and color
config.color_scheme = "Catppuccin Mocha"
config.colors = {
	background = "#181825",
}
config.font = wezterm.font("Maple Mono NF")
config.bold_brightens_ansi_colors = true
config.font_size = 12
config.freetype_load_target = "Light"

-- cursor
config.default_cursor_style = "BlinkingBlock"
config.mouse_bindings = {
	-- Ctrl-click will open the link under the mouse cursor
	{
		event = { Up = { streak = 1, button = "Left" } },
		mods = "CTRL",
		action = wezterm.action.OpenLinkAtMouseCursor,
	},
}

-- window settings
config.use_fancy_tab_bar = false
config.enable_tab_bar = false
config.window_decorations = "RESIZE"

return config
