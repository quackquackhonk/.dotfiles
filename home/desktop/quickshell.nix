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
  programs.quickshell = {
    enable = true;
    activeConfig = configs;
    systemd.enable = true;
  };
}
