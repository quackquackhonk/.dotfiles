{
  pkgs,
  config,
  lib,
  ...
}:
let
  lua = lib.generators.mkLuaInline;

  # passthrough stylix options
  border-size = config.theme.border-size;
  gaps-in = config.theme.gaps-in;
  gaps-out = config.theme.gaps-out;
  active-opacity = config.theme.active-opacity;
  inactive-opacity = config.theme.inactive-opacity;
  rounding = config.theme.rounding;
  blur = config.theme.blur;

  active-border = config.lib.stylix.colors.base0E;
  inactive-border = config.lib.stylix.colors.base01;

  animationSpeed = config.theme.animation-speed;

  animationDuration =
    if animationSpeed == "slow" then
      "4"
    else if animationSpeed == "medium" then
      "2.5"
    else
      "1.5";
  borderDuration =
    if animationSpeed == "slow" then
      "10"
    else if animationSpeed == "medium" then
      "6"
    else
      "3";

in
{
  home.packages = with pkgs; [
    xdg-desktop-portal-hyprland
    hyprpolkitagent
    hyprcursor
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    # NOTE: Until we get better docs about lua config + nix, keep this using legacy config language
    configType = "lua";

    settings = {
      # ecosystem.no_update_news = true;
      # environment variables

      # monitors
      monitor = [
        {
          output = "DP-1";
          mode = "auto";
          preferred = true;
          position = "0x0";
        }
        {
          output = "DP-3";
          mode = "1920x1080@144.00";
          position = "-1920x0";
        }
      ];

      config = {
        general = {
          gaps_in = gaps-in;
          gaps_out = gaps-out;
          border_size = border-size;

          "col.active_border" = lib.mkForce "rgb(${config.lib.stylix.colors.base04})";
          "col.inactive_border" = lib.mkForce "rgb(${config.lib.stylix.colors.base01})";

          resize_on_border = true;
          allow_tearing = false;
        };
        master = {
          new_status = "master";
        };

        misc = {
          force_default_wallpaper = 0; # Set to 0 to disable anime wallpapers
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
          disable_autoreload = true;
          focus_on_activate = true;
        };

        # cursor = {
        #   no_hardware_cursors = true;
        # };

        input = {
          kb_layout = "us";
          kb_variant = "";
          kb_model = "";
          kb_options = "";
          kb_rules = "";

          sensitivity = 0.0;
          follow_mouse = 1;
          force_no_accel = true;

          touchpad = {
            natural_scroll = false;
          };
        };

      };

      # decoration = {
      #   rounding = rounding;
      #   active_opacity = active-opacity;
      #   inactive_opacity = inactive-opacity;

      #   # https://wiki.hyprland.org/Configuring/Variables/#blur
      #   blur = {
      #     enabled = blur;
      #     size = 6;
      #     passes = 3;
      #     new_optimizations = true;
      #     ignore_opacity = true;
      #     xray = false;
      #   };
      # };

      # TODO: move from the extra config to here
      # windowrule = [
      #   # WINDOW RULES
      #   # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
      #   # Pin certain apps to workspaces
      #   "workspace 1, match:class steam, match:title .*"
      #   "workspace 1, match:class steam.*, match:title .*"
      #   "workspace 7, match:class spotify"

      #   # DMS settings menu
      #   "float on, match:class org.quickshell, match:title Settings"
      #   "center on, match:class org.quickshell, match:title Settings"

      #   ## Bluetooth manager
      #   "float on, match:class .blueman-manager-wrapped, match:title .*"
      #   "size 800 600, match:class .blueman-manager-wrapped, match:title .*"
      #   "center on, match:class .blueman-manager-wrapped, match:title .*"
      #   ## audio mixer
      #   "float on, match:class org.pulseaudio.pavucontrol, match:title .*"
      #   "size 800 600, match:class org.pulseaudio.pavucontrol, match:title .*"
      #   "center on, match:class org.pulseaudio.pavucontrol, match:title .*"

      #   ## PIP
      #   "float on, match:class zen.*, match:title Picture-in-Picture"
      #   "float on, match:class zen.*, match:title Extension:.*"
      #   ## Bitwarden
      #   ## GUI development start as floating window
      #   "float on, match:class main.exe, match:title .*"

      #   ## MISC RULES
      #   ### Ignore maximize requests from apps. You'll probably like this.
      #   "suppress_event maximize, match:class .*"
      #   ### Fix some dragging issues with XWayland
      #   "no_focus on, match:class ^$ match:title ^$ match:xwayland 1 match:float 1 match:fullscreen 0 match:pin 0"
      # ];
    };

    extraConfig = ''
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

      hl.on("hyprland.start", function()
          hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
          hl.exec_cmd("dbus-update-activation-environment --systemd --all")
          hl.exec_cmd("systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
          hl.exec_cmd("systemctl --user start hyprpolkitagent")
          hl.exec_cmd("blueman-applet")
          hl.exec_cmd("nm-applet")
          hl.exec_cmd("noctalia-shell")
          hl.exec_cmd("wl-paste --type text --watch cliphist store")
          hl.exec_cmd("wl-paste --type image --watch cliphist store")
          hl.exec_cmd("udiskie --automount --smart-tray")
          hl.exec_cmd("dms run")
          hl.exec_cmd("steam")
       end)
    '';


  };
}
