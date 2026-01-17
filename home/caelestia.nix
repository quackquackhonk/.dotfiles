{
  inputs,
  config,
  pkgs,
  ...
}:
let
  rounding = config.theme.rounding + config.theme.gaps-out;
  inherit (config.stylix) fonts;
in
{
  imports = [
    inputs.caelestia-shell.homeManagerModules.default
  ];

  home.packages = with pkgs; [
    papirus-icon-theme
    material-icons
    material-symbols
  ];

  programs.caelestia = {
    enable = true;

    systemd = {
      enable = true;
      target = "graphical-session.target";
    };

    settings = {
      appearance = {
        font = {
          family = {
            mono = fonts.monospace.name;
            sans = fonts.sansSerif.name;
            material = "Material Symbols Rounded";
          };
        };
      };

      launcher = {
        enableDangerousActions = true;
        maxShown = 10;
      };

      general = {
        apps = {
          terminal = ["ghostty"];
          audio = ["pavucontrol"];
          explorer = ["thunar"];
        };
        idle = {
          timeouts = [];
        };
      };

      background = {
        enabled = false;
      };

      border = {
        inherit rounding;
        thickness = 5;
      };

      workspaces = {
        showWindows = true;
      };

      utilities = {
        enabled = true;
        maxToasts = 4;
        toasts = {
          audioInputChanged = true;
          audioOutputChanged = true;
          capsLockChanged = false;
          chargingChanged = false;
          configLoaded = false;
          dndChanged = true;
          gameModeChanged = true;
          numLockChanged = false;
          nowPlaying = true;
          kbLayoutChanged = false;
        };
      };
      # TODO: adjust the workspaces one
      # TODO: we need to figure out the broken textures

      bar = {
        status = {
          showBattery = false;
          showAudio = true;
        };

        persistent = true;
        showOnHover = true;
        entries = [
          {
            id = "logo";
            enabled = true;
          }
          {
            id = "workspaces";
            enabled = true;
          }
          {
            id = "spacer";
            enabled = true;
          }
          {
            id = "clock";
            enabled = true;
          }
          {
            id = "spacer";
            enabled = true;
          }
          {
            id = "tray";
            enabled = true;
          }
          {
            id = "statusIcons";
            enabled = true;
          }
          {
            id = "power";
            enabled = true;
          }
        ];
      };

      sidebar = {
        enabled = true;
      };

      paths = {
        wallpaperDir = "~/dotfiles/wallpapers";
        sessionGif = ../pictures/session-gif-hacker-cat.gif;
      };

      tray = {
        background = true;
        recolour = false;
      };

    };

    cli = {
      enable = true; # Also add caelestia-cli to path
      settings = {
        theme = {
          enableTerm = false;
          enableDiscord = false;
          enableSpicetify = false;
          enableBtop = false;
          enableCava = false;
          enableHypr = false;
          enableGtk = false;
          enableQt = false;
        };
      };
    };
  };

  services.cliphist = {
    enable = true;
    allowImages = true;
  };

  wayland.windowManager.hyprland = {
    settings = {
      # bindings
      bindin = [
        # Launcher
        "$mod, mouse:272, global, caelestia:launcherInterrupt"
        "$mod, mouse:273, global, caelestia:launcherInterrupt"
        "$mod, mouse:274, global, caelestia:launcherInterrupt"
        "$mod, mouse:275, global, caelestia:launcherInterrupt"
        "$mod, mouse:276, global, caelestia:launcherInterrupt"
        "$mod, mouse:277, global, caelestia:launcherInterrupt"
        "$mod, mouse_up, global, caelestia:launcherInterrupt"
        "$mod, mouse_down, global, caelestia:launcherInterrupt"
      ];
      bind = [
        # Launcher
        "$mod, SPACE, global, caelestia:launcher"
        # TODO: I want GUI SHIFT Space to launch cli commands
        # "$mod, SPACE, global, caelestia:launcher"
        "$mod, Escape, global, caelestia:session" # Powermenu

        # Misc
        "$mod, N, exec, caelestia shell drawers toggle sidebar"

        # Utilities
        "$mod+Shift, G, exec, caelestia shell gameMode toggle" # Toggle Focus/Game mode
        # "$mod+Shift, S, global, caelestia:screenshotFreeze" # Capture region (freeze)
        # "$mod+Shift+Alt, S, global, caelestia:screenshot" # Capture region
        # "$mod+Alt, R, exec, caelestia record -s" # Record screen with sound
        # "Ctrl+Alt, R, exec, caelestia record" # Record screen
        # "$mod+Shift+Alt, R, exec, caelestia record -r" # Record region
      ];
      bindl = [
        # Media
        ", XF86AudioPlay, global, caelestia:mediaToggle"
        ", XF86AudioPause, global, caelestia:mediaToggle"
        ", XF86AudioNext, global, caelestia:mediaNext"
        ", XF86AudioPrev, global, caelestia:mediaPrev"
        ", XF86AudioStop, global, caelestia:mediaStop"

        # Sound
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"

        # Utilities
        ", Print, exec, caelestia screenshot" # Full screen capture > clipboard
      ];
      bindle = [
        ", XF86AudioRaiseVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
        ", XF86AudioLowerVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
      ];
    };
  };
}
