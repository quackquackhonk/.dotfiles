{
  inputs,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./xdg.nix
    ./bar.nix
    ./hyprland.nix
    ./binds.nix
  ];
}
