--
-- VARIABLES
--
local terminal = "ghostty"
local browser = "flatpak run app.zen_browser.zen"
local discord = "ELECTRON_OZONE_PLATFORM_HINT= discord"
local emacs = "emacsclient -c -a=''"

-- rebindings
local focus = hl.dsp.focus
local window = hl.dsp.window
local workspace = hl.dsp.workspace

local mod = "SUPER" -- Sets "Windows" key as main modifier

local exec = function(cmd)
	return hl.dsp.exec_cmd(cmd)
end
local ipc = function(cmd)
	return hl.dsp.exec_cmd("dms ipc call "  .. cmd)
end

hl.bind(mod .. " + Return", exec(terminal))
hl.bind(mod .. " + Q", hl.dsp.window.close())

hl.bind(mod .. " + E", hl.dsp.exec_cmd(emacs))
hl.bind(mod .. " + B", hl.dsp.exec_cmd(browser))

-- noctalia
hl.bind(mod .. " + Space", ipc("spotlight toggle"))
hl.bind("ALT + Space", ipc("spotlight-bar toggle"))
hl.bind(mod .. " + Escape", ipc("powermenu toggle"))
hl.bind("SUPER + V", hl.dsp.exec_cmd("dms ipc call clipboard toggle"))
hl.bind("SUPER + M", hl.dsp.exec_cmd("dms ipc call processlist focusOrToggle"))
hl.bind("SUPER + comma", hl.dsp.exec_cmd("dms ipc call settings focusOrToggle"))

-- Move focus with mod + arrow keys
hl.bind(mod .. " + left", hl.dsp.focus({ direction = "left" }))
hl.bind(mod .. " + right", hl.dsp.focus({ direction = "right" }))
hl.bind(mod .. " + up", hl.dsp.focus({ direction = "up" }))
hl.bind(mod .. " + down", hl.dsp.focus({ direction = "down" }))

-- Switch workspaces with mod + [0-9]
-- Move active window to a workspace with mod + SHIFT + [0-9]
for i = 1, 10 do
	local key = i % 10 -- 10 maps to key 0
	hl.bind(mod .. " + " .. key, hl.dsp.focus({ workspace = i }))
	hl.bind(mod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

-- Example special workspace (scratchpad)
hl.bind(mod .. " + backslash", hl.dsp.workspace.toggle_special("magic"))
hl.bind(mod .. " + SHIFT + backslash", hl.dsp.window.move({ workspace = "special:magic" }))

-- Scroll through existing workspaces with mod + scroll
hl.bind(mod .. " + bracketright", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mod .. " + bracketleft", hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mod + LMB/RMB and dragging
hl.bind(mod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- === Cheat sheet
hl.bind("SUPER + SHIFT + Slash", hl.dsp.exec_cmd("dms ipc call keybinds toggle hyprland"))

-- === Security ===
hl.bind("SUPER + ALT + L", hl.dsp.exec_cmd("dms ipc call lock lock"))
hl.bind("SUPER + SHIFT + E", hl.dsp.exit())
hl.bind("CTRL + ALT + Delete", hl.dsp.exec_cmd("dms ipc call processlist focusOrToggle"))

-- === Audio Controls ===
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("dms ipc call audio increment 3"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("dms ipc call audio decrement 3"), { locked = true, repeating = true })
hl.bind("XF86AudioMute", hl.dsp.exec_cmd("dms ipc call audio mute"), { locked = true })
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd("dms ipc call audio micmute"), { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("dms ipc call mpris playPause"), { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("dms ipc call mpris playPause"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("dms ipc call mpris previous"), { locked = true })
hl.bind("XF86AudioNext", hl.dsp.exec_cmd("dms ipc call mpris next"), { locked = true })
hl.bind("CTRL + XF86AudioRaiseVolume", hl.dsp.exec_cmd("dms ipc call mpris increment 3"), { locked = true, repeating = true })
hl.bind("CTRL + XF86AudioLowerVolume", hl.dsp.exec_cmd("dms ipc call mpris decrement 3"), { locked = true, repeating = true })

-- === Screenshots ===
hl.bind("Print", hl.dsp.exec_cmd("dms screenshot"))
hl.bind("CTRL + Print", hl.dsp.exec_cmd("dms screenshot full"))
hl.bind("ALT + Print", hl.dsp.exec_cmd("dms screenshot window"))
