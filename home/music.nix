
{
  pkgs,
  ...
}:
{
  config.home.packages = with pkgs; [
    # reaper
    ardour
    yabridge
  ];
}
