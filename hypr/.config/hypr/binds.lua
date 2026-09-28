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

local mainMod = "SUPER" -- Sets "Windows" key as main modifier

local exec = function(cmd)
	return hl.dsp.exec_cmd(cmd)
end
local ipc = function(cmd)
	return hl.dsp.exec_cmd("noctalia msg " .. cmd)
end

-- Example binds, see https://wiki.hypr.land/Configuring/Basics/Binds/ for more
hl.bind(mainMod .. " + Return", exec(terminal))
local closeWindowBind = hl.bind(mainMod .. " + Q", hl.dsp.window.close())
-- closeWindowBind:set_enabled(false)

hl.bind(mainMod .. " + E", hl.dsp.exec_cmd(emacs))
hl.bind(mainMod .. " + B", hl.dsp.exec_cmd(browser))

-- noctalia
hl.bind(mainMod .. " + Space", ipc("panel-toggle launcher"))
hl.bind(mainMod .. " + comma", ipc("settings-toggle"))
hl.bind(mainMod .. " + Escape", ipc("panel-toggle session"))

-- Move focus with mainMod + arrow keys
hl.bind(mainMod .. " + left", hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + right", hl.dsp.focus({ direction = "right" }))
hl.bind(mainMod .. " + up", hl.dsp.focus({ direction = "up" }))
hl.bind(mainMod .. " + down", hl.dsp.focus({ direction = "down" }))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
	local key = i % 10 -- 10 maps to key 0
	hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ workspace = i }))
	hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

-- Example special workspace (scratchpad)
hl.bind(mainMod .. " + backslash", hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + SHIFT + backslash", hl.dsp.window.move({ workspace = "special:magic" }))

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(mainMod .. " + bracketright", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + bracketleft", hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Laptop multimedia keys for volume and LCD brightness
hl.bind("XF86AudioRaiseVolume", ipc("volume-up"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", ipc("volume-down"), { locked = true, repeating = true })
hl.bind("XF86AudioMute", ipc("volume-mute"), { locked = true, repeating = true })

-- Requires playerctl
hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), { locked = true })
