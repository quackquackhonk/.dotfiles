-- variables
local zen = "app.zen_browser.zen"
local steam = "steam.*"
--
-- WORKSPACE DEFINITIONS
--
hl.workspace_rule({ workspace = "1", monitor = "DP-1", persistent = true, default_name = "games" })
hl.workspace_rule({ workspace = "2", monitor = "DP-1", persistent = true, default_name = "code" })
hl.workspace_rule({ workspace = "3", monitor = "DP-1", persistent = true })
hl.workspace_rule({ workspace = "4", monitor = "DP-1", persistent = true })
hl.workspace_rule({ workspace = "5", monitor = "DP-1", persistent = true })
hl.workspace_rule({ workspace = "6", monitor = "DP-3", persistent = true, default_name = "web" })
hl.workspace_rule({ workspace = "7", monitor = "DP-3", persistent = true, default_name = "music" })
hl.workspace_rule({ workspace = "8", monitor = "DP-3", persistent = true, default_name = "chat" })
hl.workspace_rule({ workspace = "9", monitor = "DP-3", persistent = true })
hl.workspace_rule({ workspace = "10", monitor = "DP-3", persistent = true })

-- Example window rules that are useful
local suppressMaximizeRule = hl.window_rule({
	-- Ignore maximize requests from all apps. You'll probably like this.
	name = "suppress-maximize-events",
	match = { class = ".*" },

	suppress_event = "maximize",
})

-- Move windows to specific workspaces
hl.window_rule({ match = { class = steam, title = ".*" }, workspace = 1 })
hl.window_rule({ match = { class = steam, title = ".*" }, workspace = 1})
hl.window_rule({ match = { class = "spotify" }, workspace = "name:music" })

-- browser pop-ups
hl.window_rule({ match = { class = zen, title = "Picture-in-Picture" }, float = true })
hl.window_rule({ match = { class = zen, title = "Extension:.*" }, float = true })

-- noctalia settings
hl.window_rule({
	match = { class = "dev.noctalia.Noctalia" },
	float = true,
	size = { 1080, 920 },
})

hl.layer_rule({
	name = "noctalia",
	match = {
		namespace = "^noctalia-(bar-.+|notification|dock|panel|attached-panel|osd|window-switcher)$",
	},
	no_anim = true,
	ignore_alpha = 0.5,
	blur = true,
	blur_popups = true,
})

hl.window_rule({
	-- Fix some dragging issues with XWayland
	name = "fix-xwayland-drags",
	match = {
		class = "^$",
		title = "^$",
		xwayland = true,
		float = true,
		fullscreen = false,
		pin = false,
	},

	no_focus = true,
})
