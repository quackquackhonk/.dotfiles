{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    prismlauncher
    (retroarch.withCores (cores: with cores; [
      dolphin
      mupen64plus
      beetle-psx-hw
    ]))
  ];
}
