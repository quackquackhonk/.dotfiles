
{
  pkgs,
  inputs,
  lib,
  config,
  ...
}:
{
  config.home.packages = with pkgs; [
    reaper
    yabridge
  ];
}
