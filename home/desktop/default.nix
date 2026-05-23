{
  inputs,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./xdg.nix
    ./caelestia.nix
    # ./dms.nix
    # ./noctalia.nix
    # ./niri.nix
    ./hyprland.nix
    ./binds.nix
  ];
}
