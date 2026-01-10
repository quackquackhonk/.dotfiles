{
  pkgs,
  config,
  lib,
  ...
}:
{
  home.packages = with pkgs; [
    grimblast
    xdg-desktop-portal-hyprland
    hyprpolkitagent
    hyprpaper
    hyprcursor
    catppuccin-cursors.mochaDark
    catppuccin-cursors.mochaLight
    tofi
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
      "$browser" = "zen-beta";
      "$files" = "thunar";
      "$menu" = "tofi-run | zsh";
      "$dmenu" = "tofi-drun | zsh";
      "$discord" = "ELECTRON_OZONE_PLATFORM_HINT= discord";
      "$emacs" = "emacsclient -c -a=''";

      # environment variables
      env = [
        "PATH,$PATH:$scrPath"
        "XDG_CURRENT_DESKTOP,Hyprland"
        # "XDG_SESSION_TYPE,wayland"
        # "XDG_SESSION_DESKTOP,Hyprland"
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
        "hyprpaper"                                                                         # wallpapers
        "hyprpanel"                                                                         # bar and notis
        "systemctl --user start hyprpolkitagent"                                            # application authentication agent
        "blueman-applet"                                                                    # bluetooth manager applet
        "nm-applet"                                                                         # networkmanager applet
        "wl-paste --type text --watch cliphist store"                                       # clipboard store text data
        "wl-paste --type image --watch cliphist store"                                      # clipboard store image data
        "emacs --daemon"                                                                    # emacs server
        "udiskie --automount --smart-tray"                                                  # auto mount USBs
        # Auto start some apps
        "[workspace 1 silent] steam"
        "[workspace 7 silent] spotify"
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
        gaps_in = 4;
        gaps_out = 4;
        border_size = 2;

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
        rounding = 0;
        active_opacity = 1.0;
        inactive_opacity = 1.0;

        # https://wiki.hyprland.org/Configuring/Variables/#blur
        blur = {
          enabled = true;
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

      # https://wiki.hyprland.org/Configuring/Variables/#gestures
      gestures = {
        workspace_swipe = false;
      };

      # keybindings
      bind =
        [
          "$mod, Q, killactive,"
          "$mod, T, togglefloating"
          # Quick launch programs
          "$mod, B, exec, $browser"
          "$mod, D, exec, $discord"
          "$mod, F, exec, $files"
          "$mod, Return, exec, $terminal"
          "$mod, Space, exec, $menu"
          "$mod SHIFT, Space, exec, $dmenu"
          "$mod, Escape, exec, wlogout"
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


    };

    extraConfig = ''
cursor:no_hardware_cursors = true
render:explicit_sync = 0

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
windowrule = workspace name:games silent, class:(steam), title:(.*)
windowrule = workspace name:games silent, class:(steam.*), title:(.*)
# windowrulev2 = workspace name:emacs silent, class:(Emacs), title:(.*)


## Bluetooth manager
windowrule = float, class:(.blueman-manager-wrapped), title:(.*)
windowrule = size 800 600, class:(.blueman-manager-wrapped), title:(.*)
windowrule = center, class:(.blueman-manager-wrapped), title:(.*)
## audio mixer
windowrule = float, class:(org.pulseaudio.pavucontrol), title:(.*)
windowrule = size 800 600, class:(org.pulseaudio.pavucontrol), title:(.*)
windowrule = center, class:(org.pulseaudio.pavucontrol), title:(.*)
## PIP
windowrule = float, class:(zen), title:(Picture-in-Picture)
windowrule = move 1286 716, class:(zen), title:(Picture-in-Picture)
## GUI development start as floating window
windowrule = float, class:(main.exe), title:(.*)

## MISC RULES
### Ignore maximize requests from apps. You'll probably like this.
windowrulev2 = suppressevent maximize, class:.*
### Fix some dragging issues with XWayland
windowrulev2 = nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0
'';

  };
}
