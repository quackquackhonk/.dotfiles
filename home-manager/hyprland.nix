{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    grimblast
    xdg-desktop-portal-hyprland
    hyprpolkitagent
    hyprcursor
    hyprpaper
    hyprlock
    wlogout
    tofi
    udiskie
    pavucontrol

    # apps
    kdePackages.dolphin
    kdePackages.qtwayland
    kdePackages.qtsvg
    kdePackages.ark
  ];

  home.file = {
    # ".config/hypr/hyprpaper.conf".source = ../hypr/hyprpaper.conf;
    ".config/wlogout" = {
      source = ../wlogout;
      recursive = true;
    };
  };

  # Optional, hint Electron apps to use Wayland:
  home.sessionVariables.NIXOS_OZONE_WL = "1";

  # hyprland config settings
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;

    settings = {
      # some global variables
      "$mod" = "SUPER";
      "$terminal" = "ghostty";
      "$browser" = "app.zen_browser.zen";
      "$files" = "dolphin";
      "$menu" = "tofi-run | zsh";

      # environment variables
      env = [
        "PATH,$PATH:$scrPath"
        "XDG_CURRENT_DESKTOP,Hyprland"
        "XDG_SESSION_TYPE,wayland"
        "XDG_SESSION_DESKTOP,Hyprland"
        "GDK_SCALE,1"

        # KDE / QT theming

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
        "hyprctl setcursor 'Catppuccin Mocha Light' 24"                                            # Mouse cursor
        "hyprpanel"                                                                         # bar and notis
        "systemctl --user start hyprpolkitagent"                                            # application authentication agent
        "blueman-applet"                                                                    # bluetooth manager applet
        "nm-applet"                                                                         # networkmanager applet
        "wl-paste --type text --watch cliphist store"                                       # clipboard store text data
        "wl-paste --type image --watch cliphist store"                                      # clipboard store image data
        "emacs --daemon"                                                                    # emacs server
        "steam"                                                                             # steam
        "udiskie --automount --smart-tray"                                                  # auto mount USBs
      ];

      # monitors
      monitor = [
        "DP-1,preffered,0x0,auto"
        "DP-2,preffered,-1920x0,auto"
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
        gaps_in = 10;
        gaps_out = 10;
        border_size = 2;

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
        rounding = 10;

        # Change transparency of focused and unfocused windows
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
          "$mod, F, exec, $files"
          "$mod, Return, exec, $terminal"
          "$mod, Space, exec, $menu"
          "$mod, Escape, exec, wlogout"
          # emacs
          "$mod, E, exec, emacsclient -c -a=''"
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
          "$mod, S, togglespecialworkspace, magic"
          "$mod SHIFT, S, movetoworkspace, special:magic"

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

# See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
# See https://wiki.hyprland.org/Configuring/Workspace-Rules/ for workspace rules

# Auto-float certain windows
windowrulev2 = float, class:(org.kde.dolphin), title:(.*)                            # File manager
windowrulev2 = center, class:(org.kde.dolphin), title:(.*)                            # File manager
# Bluetooth manager
windowrulev2 = float, class:(.blueman-manager-wrapped), title:(.*)
windowrulev2 = size 800 600, class:(.blueman-manager-wrapped), title:(.*)
windowrulev2 = center, class:(.blueman-manager-wrapped), title:(.*)
# audio mixer
windowrulev2 = float, class:(org.pulseaudio.pavucontrol), title:(.*)
windowrulev2 = size 800 600, class:(org.pulseaudio.pavucontrol), title:(.*)
windowrulev2 = center, class:(org.pulseaudio.pavucontrol), title:(.*)
# PIP
windowrulev2 = float, class:(zen), title:(Picture-in-Picture)
windowrulev2 = move 1286 716, class:(zen), title:(Picture-in-Picture)
# gui development start as float
windowrulev2 = float, class:(main.exe), title:(.*)

# Ignore maximize requests from apps. You'll probably like this.
windowrulev2 = suppressevent maximize, class:.*

# Fix some dragging issues with XWayland
windowrulev2 = nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0
'';

  };
}
