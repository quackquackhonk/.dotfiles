{
  inputs,
  lib,
  ...
}:
{
  imports = [
    inputs.dms.homeModules.dank-material-shell
  ];

  # NOTE: I would love to go full custom quickshell

  programs.dank-material-shell = {
    enable = true;

    systemd = {
      enable = true;
      restartIfChanged = true;
    };

    # Core features
    enableSystemMonitoring = true; # System monitoring widgets (dgop)
    enableVPN = true; # VPN management widget
    enableDynamicTheming = false; # Wallpaper-based theming (matugen)
    enableAudioWavelength = true; # Audio visualizer (cava)
    enableCalendarEvents = true; # Calendar integration (khal)
    enableClipboardPaste = true; # Pasting items from the clipboard (wtype)

    settings = {
      configVersion = 5;

      # Themes, override stylix
      currentThemeName = lib.mkForce "custom";
      currentThemeCategory = lib.mkForce "custom";
      customThemeFile = lib.mkForce ../../dms/catppuccin.json;
      registryThemeVariants = {
        catppuccin = {
          dark = {
            flavor = "mocha";
            accent = "mauve";
          };
        };
      };

      widgetBackgroundColor = "sth";
      widgetColorMode = "colorful";

      fontFamily = lib.mkForce "Maple Mono NF";
      monoFontFamily = "Maple Mono NF";
      blurEnabled = true;
      use24HourClock = false;
      useFarenheit = true;

      # Workspaces
      showWorkspaceIndex = false;
      showWorkspaceApps = false;
      workspaceColorMode = "pri";
      workspaceOccupiedColorMode = "sec";
      workspaceUnfocusedColorMode = "s";

      # launcher
      launcherLogoMode = "os";

      # on screen display
      osdAlwaysShowValue = true;

      barConfigs = [
        {
          id = "default";
          name = "Main";
          enabled = true;
          position = 0;
          screenPreferences = [ "all" ];
          showOnLastDisplay = true;
          leftWidgets = [
            "launcherButton"
            "workspaceSwitcher"
            {
              id = "focusedWindow";
              enabled = true;
              focusedWindowCompactMode = true;
            }
          ];
          centerWidgets = [
            "clock"
            "music"
          ];
          rightWidgets = [
            "notificationButton"
            "systemTray"
            "controlCenterButton"
            "powerMenuButton"
          ];
          spacing = 4;
          innerPadding = 4;
          bottomGap = 0;
          transparency = 1.0;
          widgetTransparency = 1.0;
          squareCorners = false;
          noBackground = false;
          maximizeWidgetIcons = true;
          maximizeWidgetText = false;
          removeWidgetPadding = false;
          widgetPadding = 8;
          gothCornersEnabled = false;
          gothCornerRadiusOverride = false;
          gothCornerRadiusValue = 12;
          borderEnabled = false;
          borderColor = "surfaceText";
          borderOpacity = 1.0;
          borderThickness = 1;
          widgetOutlineEnabled = false;
          widgetOutlineColor = "primary";
          widgetOutlineOpacity = 1.0;
          widgetOutlineThickness = 1;
          fontScale = 1.25;
          iconScale = 1.0;
          autoHide = false;
          autoHideStrict = false;
          autoHideDelay = 250;
          showOnWindowsOpen = false;
          openOnOverview = false;
          visible = true;
          popupGapsAuto = true;
          popupGapsManual = 4;
          maximizeDetection = true;
          useOverlayLayer = false;
          scrollEnabled = true;
          scrollXBehavior = "column";
          scrollYBehavior = "workspace";
          shadowIntensity = 0;
          shadowOpacity = 60;
          shadowColorMode = "default";
          shadowCustomColor = "#000000";
          clickThrough = false;
        }
      ];
    };
  };
}
