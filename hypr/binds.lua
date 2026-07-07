--
-- VARIABLES
--
local mod = "SUPER"
local terminal = "ghostty"
local browser = "zen-twilight"
local discord = "ELECTRON_OZONE_PLATFORM_HINT= discord"
local emacs = "emacsclient -c -a=''"

-- rebindings
local focus = hl.dsp.focus
local window = hl.dsp.window
local workspace = hl.dsp.workspace

function exec(cmd)
	return hl.dsp.exec_cmd(cmd)
end

function dms(cmd)
	return hl.dsp.exec_cmd("dms ipc call " .. cmd)
end

-- general bindings
hl.bind(mod .. " + Q", window.close())
hl.bind(mod .. " + SHIFT + Q", window.kill())
hl.bind(mod .. " + F", window.float())
hl.bind(mod .. " + F11", window.fullscreen({ mode = "fullscreen", action = "toggle" }))

-- launcher
hl.bind(mod .. " + SPACE", dms("launcher toggle"))
hl.bind(mod .. " + Escape", dms("powermenu toggle"))
-- apps
hl.bind(mod .. " + Return", exec(terminal))
hl.bind(mod .. " + E", exec(emacs))
hl.bind(mod .. " + SHIFT + E", exec("emacsclient -e '(kill-emacs)'"))
hl.bind(mod .. " + B", exec(browser))

-- Move focus with mod + arrow, move with mod,SHIFT+arrow
hl.bind(mod .. " + left", focus({ direction = "left" }))
hl.bind(mod .. " + right", focus({ direction = "right" }))
hl.bind(mod .. " + up", focus({ direction = "up" }))
hl.bind(mod .. " + down", focus({ direction = "down" }))
hl.bind(mod .. " + SHIFT + left", window.move({ direction = "left" }))
hl.bind(mod .. " + SHIFT + right", window.move({ direction = "right" }))
hl.bind(mod .. " + SHIFT + up", window.move({ direction = "up" }))
hl.bind(mod .. " + SHIFT + down", window.move({ direction = "down" }))

-- Switch workspaces with mod + [0-9]
-- Move active window to a workspace with mod + SHIFT + [0-9]
for i = 1, 10 do
	local key = i % 10 -- 10 maps to key 0
	hl.bind(mod .. " + " .. key, focus({ workspace = i }))
	hl.bind(mod .. " + SHIFT + " .. key, window.move({ workspace = i }))
end
-- move windows with mod + drag
hl.bind(mod .. " + mouse:272", window.drag(),   { mouse = true })
hl.bind(mod .. " + mouse:273", window.resize(), { mouse = true })
-- Scroll workspaces with mod + [/]
hl.bind(mod .. " + bracketright", window.move({workspace = "e+1"}))
hl.bind(mod .. " + SHIFT + bracketright", focus({workspace = "e+1"}))
hl.bind(mod .. " + bracketleft", focus({workspace = "e-1"}))
hl.bind(mod .. " + SHIFT + bracketleft", window.move({workspace = "e-1"}))

-- Example special workspace (scratchpad)
hl.bind(mod .. " + S", workspace.toggle_special("magic"))
hl.bind(mod .. " + SHIFT + S", window.move({ workspace = "special:magic" }))
