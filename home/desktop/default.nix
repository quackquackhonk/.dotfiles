{
  inputs,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./xdg.nix
    ./dms.nix
    # ./quickshell.nix
    ./hyprland.nix
    ./binds.nix
  ];

  home.packages = with pkgs; [
    grimblast
    catppuccin-cursors.mochaDark
    catppuccin-cursors.mochaLight
    rofi
    udiskie
    pavucontrol
  ];

}
