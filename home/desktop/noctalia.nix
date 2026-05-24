{
  pkgs,
  inputs,
  config,
  lib,
  ...
}:
{
  imports = [
    inputs.noctalia.homeModules.default
  ];

  programs.noctalia-shell = {
    enable = true;
  };
}
