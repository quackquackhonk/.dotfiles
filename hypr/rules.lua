--
-- WORKSPACE DEFINITIONS
--
hl.workspace_rule({ workspace = "1", monitor = "DP-1", persistent = true, default_name = "games" })
hl.workspace_rule({ workspace = "2", monitor = "DP-1", persistent = true, default_name = "emacs" })
hl.workspace_rule({ workspace = "3", monitor = "DP-1", persistent = true })
hl.workspace_rule({ workspace = "4", monitor = "DP-1", persistent = true })
hl.workspace_rule({ workspace = "5", monitor = "DP-1", persistent = true })
hl.workspace_rule({ workspace = "6", monitor = "DP-3", persistent = true, default_name = "browser" })
hl.workspace_rule({ workspace = "7", monitor = "DP-3", persistent = true, default_name = "music" })
hl.workspace_rule({ workspace = "8", monitor = "DP-3", persistent = true, default_name = "discord" })
hl.workspace_rule({ workspace = "9", monitor = "DP-3", persistent = true })
hl.workspace_rule({ workspace = "0", monitor = "DP-3", persistent = true })

-- Move windows to specific workspaces
hl.window_rule({ match = { class = "steam", title = ".*" }, workspace = "name:games" })
hl.window_rule({ match = { class = "steam.*", title = ".*" }, workspace = "name:games" })
hl.window_rule({ match = { class = "spotify" }, workspace = "name:music" })

-- Auto-float windows
-- DMS settings
hl.window_rule({ match = { class = "org.quickshell", title = "Settings" }, float = true, center = true })
-- bluetooth
hl.window_rule({ match = { class = ".blueman-manager-wrapped", title = ".*" }, float = true, center = true })
-- audio
hl.window_rule({ match = { class = "org.pulseaudio.pavucontrol", title = ".*" }, float = true, center = true })

-- browser pop-ups
hl.window_rule({ match = { class = "zen.*", title = "Picture-in-Picture" }, float = true })
hl.window_rule({ match = { class = "zen.*", title = "Extension:.*" }, float = true })
