-- This is an example Hyprland Lua config file.
-- Refer to the wiki for more information.
-- https://wiki.hypr.land/Configuring/Start/

-- Please note not all available settings / options are set here.
-- For a full list, see the wiki

-- You can (and should!!) split this configuration into multiple files
-- Create your files separately and then require them like this:
-- require("myColors")

---------------------
---- MY PROGRAMS ----
---------------------

-- Set programs that you use
local terminal = "ghostty"
local fileManager = "dolphin"
local browser = "flatpak run app.zen_browser.zen"
local emacs = "emacsclient -c -a=''"


-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
-- hl.env("HYPRCURSOR_NAME", "'Catppuccin Mocha Light'")
-- hl.env("HYPRCURSOR_SIZE", "16")
-- hl.env("XCURSOR_NAME", "'Catppuccin Mocha Light'")
-- hl.env("XCURSOR_SIZE", "16")
-- for nvidia
hl.env("LIBVA_DRIVER_NAME", "nvidia")
hl.env("__GLX_VENDOR_LIBRARY_NAME", "nvidia")
hl.env("__GL_VRR_ALLOWED", "1")
hl.env("WLR_DRM_NO_ATOMIC", "1")


-------------------
---- AUTOSTART ----
-------------------

-- See https://wiki.hypr.land/Configuring/Basics/Autostart/

-- Autostart necessary processes (like notifications daemons, status bars, etc.)
-- Or execute your favorite apps at launch like this:
--
hl.on("hyprland.start", function()
	hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP") -- for XDPH
	hl.exec_cmd("dbus-update-activation-environment --systemd --all") -- for XDPH
	hl.exec_cmd("systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP") -- for XDPH
	hl.exec_cmd("systemctl --user start hyprpolkitagent") -- application authentication agent
	hl.exec_cmd("blueman-applet") -- bluetooth manager applet
	hl.exec_cmd("nm-applet") -- networkmanager applet
	hl.exec_cmd("wl-paste --type text --watch cliphist store") -- clipboard store text data
	hl.exec_cmd("wl-paste --type image --watch cliphist store") -- clipboard store image data
	hl.exec_cmd("noctalia")

	-- Auto start some apps
    hl.exec_cmd("steam")
	hl.exec_cmd(browser)
	hl.exec_cmd(emacs)
end)
-----------------------
---- LOOK AND FEEL ----
-----------------------

-- Refer to https://wiki.hypr.land/Configuring/Basics/Variables/
hl.config({
	general = {
		gaps_in = 5,
		gaps_out = 10,
	},

	decoration = {
		rounding = 20,
		rounding_power = 2,

		shadow = {
			enabled = true,
			range = 4,
			render_power = 3,
			color = 0xee1a1a1a,
		},

		blur = {
			enabled = true,
			size = 3,
			passes = 2,
			vibrancy = 0.1696,
		},
	},

	animations = {
		enabled = true,
	},

	cursor = {
		no_hardware_cursors = 1,
	},
})

-- See https://wiki.hypr.land/Configuring/Layouts/Dwindle-Layout/ for more
hl.config({
	dwindle = {
		preserve_split = true, -- You probably want this
	},
})

-- See https://wiki.hypr.land/Configuring/Layouts/Master-Layout/ for more
hl.config({
	master = {
		new_status = "master",
	},
})

-- See https://wiki.hypr.land/Configuring/Layouts/Scrolling-Layout/ for more
hl.config({
	scrolling = {
		fullscreen_on_one_column = true,
	},
})

----------------
----  MISC  ----
----------------

hl.config({
	misc = {
		force_default_wallpaper = -1, -- Set to 0 or 1 to disable the anime mascot wallpapers
		disable_hyprland_logo = false, -- If true disables the random hyprland logo / anime girl background. :(
	},
})

---------------
---- INPUT ----
---------------

hl.config({
	input = {
		kb_layout = "us",
		kb_variant = "",
		kb_model = "",
		kb_options = "",
		kb_rules = "",

		follow_mouse = 1,

		sensitivity = 0, -- -1.0 - 1.0, 0 means no modification.

		touchpad = {
			natural_scroll = false,
		},
	},
})

---------------------
---- KEYBINDINGS ----
---------------------
require('binds')


--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------
require('rules')


-- For Noctalia Color templates
require("noctalia").apply_theme()
