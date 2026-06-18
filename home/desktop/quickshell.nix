{
  inputs,
  pkgs,
  ...
}:
let
  configs = builtins.path {
    path = ../../quickshell;
    name = "qqh-quickshell-configs";
  };
in
{
  home.packages = with pkgs; [
    quickshell
    qt6.qtwayland
  ];
  programs.quickshell = {
    enable = true;
    activeConfig = configs;
    systemd.enable = true;
  };
}
