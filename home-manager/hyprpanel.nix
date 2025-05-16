{
  ...
}:
{
  programs.hyprpanel = {
    enable = true;

    settings = {
      # Main bar layout
      layout = {
        "bar.layouts" = {
          "*" = {
            "left" = [
              "windowtitle"
              "network"
              "bluetooth"
              "volume"
            ];
            "middle" = [
              "workspaces"
            ];
            "right" = [
              "systray"
              "clock"
            ];
          };
        };
      };

      menus = {
        power.confirmation = false;
      };

      bar = {
        autoHide = "fullscreen";

        network = {
          truncation_size = 20;
        };
      };

      theme = {
        bar = {
          buttons = {
            workspaces = {
              pill = {
                active_width = "8em";
              };
              fontSize = "1.0em";
            };
          };
          outer_spacing = "0.5em";
          floating = true;
          transparent = true;
        };
        # On screen Display
        osd = {
          location = "bottom";
          orientation = "horizontal";
        };

        font = {
          name = "Iosevka Nerd Font Mono";
          size = "14px";
        };
      };
    };
  };
}
