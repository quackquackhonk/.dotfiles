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
          terminal = [ "ghostty" ];
          audio = [ "pavucontrol" ];
          explorer = [ "thunar" ];
        };
        idle = {
          timeouts = [ ];
        };
      };

      background = {
        enabled = false;
      };

      border = {
        inherit rounding;
        thickness = 5;
      };

      # workspaces = {
      #   showWindows = true;
      # };

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
          nowPlaying = false;
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
            id = "activeWindow";
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

        tray = {
          background = true;
          recolour = false;
        };

      };

      sidebar = {
        enabled = true;
      };

      paths = {
        sessionGif = ../../pictures/session-gif-hacker-cat.gif;
      };

    };

    cli = {
      enable = true; # Also add caelestia-cli to path
      settings = {
        theme = {
          enableTerm = false;
          enableDiscord = true;
          enableSpicetify = true;
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
}
