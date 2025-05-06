{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    nwg-look
  ];

  stylix.targets = {
    gtk.enable = false;
  };
}
