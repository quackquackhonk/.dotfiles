--
-- MONITORS
--
hl.monitor({
	output = "DP-1",
	mode = "preferred",
	position = "0x0",
	scale = "auto",
})
hl.monitor({
	output = "DP-3",
	mode = "preferred",
	position = "-1920x0",
	scale = "auto",
})

--
-- ENVIRONMENT VARIABLES
--
hl.env("PATH", "$PATH:$scrPath")
hl.env("XDG_CURRENT_DESKTOP", "Hyprland")
hl.env("GDK_SCALE", "1")
hl.env("HYPRCURSOR_NAME", "'Catppuccin Mocha Light'")
hl.env("HYPRCURSOR_SIZE", "16")
hl.env("XCURSOR_NAME", "'Catppuccin Mocha Light'")
hl.env("XCURSOR_SIZE", "16")
hl.env("SUDO_ASKPASS", "hyprpolkitagent")
-- for nvidia
hl.env("LIBVA_DRIVER_NAME", "nvidia")
hl.env("__GLX_VENDOR_LIBRARY_NAME", "nvidia")
hl.env("__GL_VRR_ALLOWED", "1")
hl.env("WLR_DRM_NO_ATOMIC", "1")

--
-- AUTOSTART
--
local terminal = "ghostty"
local browser = "zen-twilight"
local discord = "ELECTRON_OZONE_PLATFORM_HINT= discord"
local emacs = "emacsclient -c -a=''"
hl.on("hyprland.start", function()
	hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP") -- for XDPH
	hl.exec_cmd("dbus-update-activation-environment --systemd --all") -- for XDPH
	hl.exec_cmd("systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP") -- for XDPH
	hl.exec_cmd("systemctl --user start hyprpolkitagent") -- application authentication agent
	hl.exec_cmd("blueman-applet") -- bluetooth manager applet
	hl.exec_cmd("nm-applet") -- networkmanager applet
	hl.exec_cmd("noctalia-shell") -- start the bar
	hl.exec_cmd("wl-paste --type text --watch cliphist store") -- clipboard store text data
	hl.exec_cmd("wl-paste --type image --watch cliphist store") -- clipboard store image data
	hl.exec_cmd("udiskie --automount --smart-tray") -- auto mount USBs

	-- Auto start some apps
	hl.exec_cmd("steam")
	hl.exec_cmd(browser)
	hl.exec_cmd("spotify")
	hl.exec_cmd(discord)
    hl.exec_cmd(emacs)
end)

--
-- LOOK AND FEEL
--
require("qqh/stylix")

hl.config({
	dwindle = {
		preserve_split = true,
	},

	master = {
		new_status = "master",
	},

	misc = {
		force_default_wallpaper = -1, -- Set to 0 or 1 to disable the anime mascot wallpapers
		disable_hyprland_logo = false, -- If true disables the random hyprland logo / anime girl background. :(
	},
})

-- See https://wiki.hypr.land/Configuring/Layouts/Scrolling-Layout/ for more
hl.config({
	scrolling = {
		fullscreen_on_one_column = true,
	},
})


--
-- INPUT
--
hl.config({
	input = {
		kb_layout = "us",
		kb_variant = "",
		kb_model = "",
		kb_options = "",
		kb_rules = "",

		follow_mouse = 1,
		sensitivity = 0, -- -1.0 - 1.0, 0 means no modification.
		force_no_accel = true,

		touchpad = {
			natural_scroll = false,
		},
	},
})

hl.config({
	cursor = { no_hardware_cursors = true },
})


--
-- KEYBINDINGS
--
require('qqh/binds')

--
-- WINDOW RULES
--
require('qqh/rules')
