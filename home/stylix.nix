{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    nwg-look
  ];

  stylix.targets = {
    hyprland.enable = true;
    hyprland.hyprpaper.enable = true;
  };
}
