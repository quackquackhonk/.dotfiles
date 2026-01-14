{
  pkgs,
  config,
  lib,
  ...
}:
let
  border-size = config.theme.border-size;
  gaps-in = config.theme.gaps-in;
  gaps-out = config.theme.gaps-out;
  active-opacity = config.theme.active-opacity;
  inactive-opacity = config.theme.inactive-opacity;
  rounding = config.theme.rounding;
  blur = config.theme.blur;

  background = "rgba(" + config.lib.stylix.colors.base00 + "77)";
in
{
  home.packages = with pkgs; [
    grimblast
    xdg-desktop-portal-hyprland
    hyprpolkitagent
    hyprpaper
    hyprcursor
    catppuccin-cursors.mochaDark
    catppuccin-cursors.mochaLight
    rofi
    udiskie
    pavucontrol
  ];
  # hyprland config settings
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;

    settings = {
      # some global variables
      "$mod" = "SUPER";
      "$terminal" = "ghostty";
      "$browser" = "zen-twilight";
      "$menu" = "tofi-run | zsh";
      "$dmenu" = "tofi-drun | zsh";
      "$togglebar" = "hyprpanel toggleWindow bar-0";
      "$discord" = "ELECTRON_OZONE_PLATFORM_HINT= discord";
      "$emacs" = "emacsclient -c -a=''";

      # environment variables
      env = [
        "PATH,$PATH:$scrPath"
        "XDG_CURRENT_DESKTOP,Hyprland"
        "GDK_SCALE,1"
        "HYPRCURSOR_NAME,'Catppuccin Mocha Light'"
        "HYPRCURSOR_SIZE,24"
        "XCURSOR_NAME,'Catppuccin Mocha Light'"
        "XCURSOR_SIZE,24"
        "SUDO_ASKPASS,hyprpolkitagent"

        # for nvidia
        "LIBVA_DRIVER_NAME,nvidia"
        "__GLX_VENDOR_LIBRARY_NAME,nvidia"
        "__GL_VRR_ALLOWED,1"
        "WLR_DRM_NO_ATOMIC,1"
      ];

      # startup applications
      exec-once = [
        "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"  # for XDPH
        "dbus-update-activation-environment --systemd --all"                                # for XDPH
        "systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"           # for XDPH
        "systemctl --user start hyprpolkitagent"                                            # application authentication agent
        "blueman-applet"                                                                    # bluetooth manager applet
        "nm-applet"                                                                         # networkmanager applet
        "wl-paste --type text --watch cliphist store"                                       # clipboard store text data
        "wl-paste --type image --watch cliphist store"                                      # clipboard store image data
        "udiskie --automount --smart-tray"                                                  # auto mount USBs
        # Auto start some apps
        "[workspace 1 silent] steam"
        "[workspace 6] $browser"
      ];

      # monitors
      monitor = [
        "DP-1,preffered,0x0,auto"
        "DP-2,preffered,2560x0,auto"
      ];

      # some workspace rules
      workspace = [
        "1, monitor:DP-1, persistent:true"
        "2, monitor:DP-1, persistent:true"
        "3, monitor:DP-1, persistent:true"
        "4, monitor:DP-1, persistent:true"
        "5, monitor:DP-1, persistent:true"
        "6, monitor:DP-2, persistent:true"
        "7, monitor:DP-2, persistent:true"
        "8, monitor:DP-2, persistent:true"
        "9, monitor:DP-2, persistent:true"
        "10, monitor:DP-2, persistent:true"
      ];


      general = {
        gaps_in = gaps-in;
        gaps_out = gaps-out;
        border_size = border-size;

        "col.active_border" = lib.mkForce "rgb(${config.lib.stylix.colors.base04})";
        "col.inactive_border" = lib.mkForce "rgb(${config.lib.stylix.colors.base01})";

        resize_on_border = true;
        allow_tearing = false;
        layout = "dwindle";
      };

      animations = {
        enabled = true;
        bezier = [
          "wind, 0.05, 0.9, 0.1, 1.0"
          "winIn, 0.1, 1.1, 0.1, 1.0"
          "winOut, 0.3, -0.3, 0, 1"
          "liner, 1, 1, 1, 1"
        ];
        animation = [
          "windows, 1, 8, wind, popin"
          "windowsIn, 1, 8, winIn, popin"
          "windowsOut, 1, 7, winOut, popin"
          "windowsMove, 1, 7, wind, popin"
          "border, 1, 1, liner"
          "borderangle, 1, 30, liner, loop"
          "fade, 1, 10, default"
          "workspaces, 1, 7, wind, fade"
        ];
      };

      decoration = {
        rounding = rounding;
        active_opacity = active-opacity;
        inactive_opacity = inactive-opacity;

        # https://wiki.hyprland.org/Configuring/Variables/#blur
        blur = {
          enabled = blur;
          size = 6;
          passes = 3;
          new_optimizations = true;
          ignore_opacity = true;
          xray = false;
        };
      };

      dwindle = {
        pseudotile = true;
        preserve_split = true;
      };

      master = {
        new_status = "master";
      };

      misc = {
        force_default_wallpaper = 0; # Set to 0 to disable anime wallpapers
        vfr = true;
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
        disable_autoreload = true;
        focus_on_activate = true;
      };

      cursor = {
        no_hardware_cursors = true;
      };

      input = {
        kb_layout = "us";
        kb_variant = "";
        kb_model = "";
        kb_options = "";
        kb_rules = "";

        sensitivity = -0.2;
        follow_mouse = 1;
        force_no_accel = true;

        touchpad = {
          natural_scroll = false;
        };
      };

      # keybindings
      bind =
        [
          "$mod, Q, killactive,"
          "$mod, T, togglefloating"
          # Quick launch programs
          "$mod, B, exec, $browser"
          "$mod, D, exec, $discord"
          "$mod, Return, exec, $terminal"
          "$mod, Space, exec, $menu"
          "$mod SHIFT, Space, exec, $dmenu"
          "$mod, W, exec, $togglebar"

          # emacs
          "$mod, E, exec, $emacs"
          "$mod SHIFT, E, exec, emacsclient -e '(kill-emacs)'"

          # Move focus with arrow keys
          "$mod, left, movefocus, l"
          "$mod, right, movefocus, r"
          "$mod, up, movefocus, u"
          "$mod, down, movefocus, d"

          # Switch to a relative workspace
          "$mod, bracketright, workspace, r+1"
          "$mod, bracketleft, workspace, r-1"

          # Move focused window to a relative workspace
          "$mainMod+Alt, rightbracket, movetoworkspace, r+1"
          "$mainMod+Alt, leftbracket, movetoworkspace, r-1"

          # special workspace (scratchpad)
          "$mod, backslash, togglespecialworkspace, magic"
          "$mod SHIFT, backslash, movetoworkspace, special:magic"

          ", Print, exec, grimblast copy area"
        ]
        ++ (
          # workspaces
          # binds $mod + [shift +] {1..9} to [move to] workspace {1..9}
          builtins.concatLists (builtins.genList (i:
            let ws = i + 1;
            in [
              "$mod, code:1${toString i}, workspace, ${toString ws}"
              "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
            ]
          )
            9)
        );

      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];

      bindel = [
        ",XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
        ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
        ",XF86MonBrightnessUp, exec, brightnessctl s 10%+"
        ",XF86MonBrightnessDown, exec, brightnessctl s 10%-"
      ];

      bindl = [
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPause, exec, playerctl play-pause"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPrev, exec, playerctl previous"
      ];

      # TODO: move from the extra config to here
      # windowrule = [];

    };


    extraConfig = ''
#
# WORKSPACE RULES
# See https://wiki.hyprland.org/Configuring/Workspace-Rules/ for workspace rules
#
# Set workspace names
workspace = 1, defaultName:games
workspace = 2, defaultName:emacs
workspace = 6, defaultName:browser
workspace = 7, defaultName:discord

#
# WINDOW RULES
# See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
#

# Pin certain apps to workspaces
windowrule = workspace name:games silent, match:class steam match:title .*
windowrule = workspace name:games silent, match:class steam.* match:title .*

## Bluetooth manager
windowrule = float on, match:class .blueman-manager-wrapped match:title .*
windowrule = size 800 600, match:class .blueman-manager-wrapped, match:title .*
windowrule = center on, match:class .blueman-manager-wrapped, match:title .*
## audio mixer
windowrule = float on, match:class org.pulseaudio.pavucontrol, match:title .*
windowrule = size 800 600, match:class org.pulseaudio.pavucontrol, match:title .*
windowrule = center on, match:class org.pulseaudio.pavucontrol, match:title .*

## PIP
windowrule = float on, match:class zen, match:title Picture-in-Picture
## GUI development start as floating window
windowrule = float on, match:class main.exe match:title .*

## MISC RULES
### Ignore maximize requests from apps. You'll probably like this.
windowrule = suppress_event maximize, match:class .*
### Fix some dragging issues with XWayland
windowrule = no_focus on, match:class ^$ match:title ^$ match:xwayland 1 match:float 1 match:fullscreen 0 match:pin 0
'';

  };
}
