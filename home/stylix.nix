{
  pkgs,
  inputs,
  lib,
  config,
  ...
}:

let
  fontSize = 12;
in
{
  # imports = with inputs.stylix.nixosModules; [ stylix ];
  options.theme = lib.mkOption {
    type = lib.types.attrs;
    default = {
      rounding = 10;
      gaps-in = 5;
      gaps-out = 10;
      active-opacity = 1.0;
      inactive-opacity = 0.95;
      blur = true;
      border-size = 2;
      animation-speed = "medium"; # "fast" | "medium" | "slow"
      fetch = "none"; # "nerdfetch" | "neofetch" | "pfetch" | "none"
      textColorOnWallpaper = config.lib.stylix.colors.base00; # Color of the text displayed on the wallpaper (Lockscreen, display manager, ...)
    };
    description = "Theme configuration options";
  };

  config.home.packages = with pkgs; [
    nwg-look
  ];

  config.stylix = {
    enable = true;
    autoEnable = true;
    image = ../wallpapers/space.png;
    polarity = "dark";

    targets = {
      hyprland.enable = true;
      hyprland.hyprpaper.enable = true;
      qt.enable = true;
      gtk.enable = true;
    };

    base16Scheme = {
      system = "base16";
      name = "Catppuccin Mocha";
      author = "https://github.com/catppuccin/catppuccin";
      variant = "dark";

      palette = {
        base00 = "1e1e2e"; # base
        base01 = "181825"; # mantle
        base02 = "313244"; # surface0
        base03 = "45475a"; # surface1
        base04 = "585b70"; # surface2
        base05 = "cdd6f4"; # text
        base06 = "f5e0dc"; # rosewater
        base07 = "b4befe"; # lavender
        base08 = "f38ba8"; # red
        base09 = "fab387"; # peach
        base0A = "f9e2af"; # yellow
        base0B = "a6e3a1"; # green
        base0C = "94e2d5"; # teal
        base0D = "89b4fa"; # blue
        base0E = "cba6f7"; # mauve
        base0F = "f2cdcd"; # flamingo
      };
    };

    fonts = {

      monospace = {
        package = pkgs.maple-mono.NF-unhinted;
        name = "Maple Mono NF";
      };
      serif = config.stylix.fonts.sansSerif;
      sansSerif = {
        package = pkgs.source-sans-pro;
        name = "Source Sans Pro";
      };
      emoji = {
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };

      sizes = {
        applications = fontSize;
        desktop = fontSize;
        popups = fontSize;
        terminal = fontSize;
      };
    };
  };
}
