--
-- VARIABLES
--
local terminal = "ghostty"
local browser = {class = "app.zen_browser.zen", cmd = "flatpak run app.zen_browser.zen"}
local emacs = {class = "Emacs", cmd = "emacsclient -c -a=''"}
local spotify = {class = "Spotify", cmd = "spotify"}
local steam = {class = "steam", cmd = "steam"}

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

local function run_or_raise(class_name, launch_cmd)
    local windows = hl.get_windows()

    for _, win in ipairs(windows) do
        if win.class == class_name then
            return hl.dsp.focus({window = "address:" .. win.address})
        end
    end

    if not found then
        return hl.dsp.exec_cmd(launch_cmd)
    end
end

local function app_binding(key, app)
    hl.bind(mod .. " + " .. key, run_or_raise(app.class, app.cmd))
    hl.bind(mod .. " + SHIFT + " .. key, exec(app.cmd))
end


hl.bind(mod .. " + Q", hl.dsp.window.close())
hl.bind(mod .. " + Return", exec(terminal))

app_binding("G", steam)
app_binding("E", emacs)
app_binding("B", browser)
-- FIXME: this one doesnt focus
app_binding("S", spotify)

-- noctalia
hl.bind(mod .. " + Space", ipc("spotlight toggle"))
hl.bind("ALT + Space", ipc("spotlight-bar toggle"))
hl.bind(mod .. " + Escape", ipc("powermenu toggle"))
hl.bind("SUPER + V", exec("dms ipc call clipboard toggle"))
hl.bind("SUPER + M", exec("dms ipc call processlist focusOrToggle"))
hl.bind("SUPER + comma", exec("dms ipc call settings focusOrToggle"))

-- Move focus with mod + arrow keys
hl.bind(mod .. " + left", focus({ direction = "left" }))
hl.bind(mod .. " + right", focus({ direction = "right" }))
hl.bind(mod .. " + up", focus({ direction = "up" }))
hl.bind(mod .. " + down", focus({ direction = "down" }))

-- Switch workspaces with mod + [0-9]
-- Move active window to a workspace with mod + SHIFT + [0-9]
for i = 1, 10 do
	local key = i % 10 -- 10 maps to key 0
	hl.bind(mod .. " + " .. key, focus({ workspace = i }))
	hl.bind(mod .. " + SHIFT + " .. key, window.move({ workspace = i }))
end

-- Example special workspace (scratchpad)
hl.bind(mod .. " + backslash", workspace.toggle_special("magic"))
hl.bind(mod .. " + SHIFT + backslash", window.move({ workspace = "special:magic" }))

-- Scroll through existing workspaces with mod + scroll
hl.bind(mod .. " + bracketright", focus({ workspace = "e+1" }))
hl.bind(mod .. " + bracketleft", focus({ workspace = "e-1" }))

-- Move/resize windows with mod + LMB/RMB and dragging
hl.bind(mod .. " + mouse:272", window.drag(), { mouse = true })
hl.bind(mod .. " + mouse:273", window.resize(), { mouse = true })

-- === Security ===
hl.bind("SUPER + ALT + L", exec("dms ipc call lock lock"))
hl.bind("CTRL + ALT + Delete", exec("dms ipc call processlist focusOrToggle"))

-- === Audio Controls ===
hl.bind("XF86AudioRaiseVolume", exec("dms ipc call audio increment 3"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", exec("dms ipc call audio decrement 3"), { locked = true, repeating = true })
hl.bind("XF86AudioMute", exec("dms ipc call audio mute"), { locked = true })
hl.bind("XF86AudioMicMute", exec("dms ipc call audio micmute"), { locked = true })
hl.bind("XF86AudioPause", exec("dms ipc call mpris playPause"), { locked = true })
hl.bind("XF86AudioPlay", exec("dms ipc call mpris playPause"), { locked = true })
hl.bind("XF86AudioPrev", exec("dms ipc call mpris previous"), { locked = true })
hl.bind("XF86AudioNext", exec("dms ipc call mpris next"), { locked = true })
hl.bind("CTRL + XF86AudioRaiseVolume", exec("dms ipc call mpris increment 3"), { locked = true, repeating = true })
hl.bind("CTRL + XF86AudioLowerVolume", exec("dms ipc call mpris decrement 3"), { locked = true, repeating = true })

-- === Screenshots ===
hl.bind("Print", exec("dms screenshot"))
hl.bind("CTRL + Print", exec("dms screenshot full"))
hl.bind("ALT + Print", exec("dms screenshot window"))
