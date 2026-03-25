{
  pkgs,
  ...
}:
{
  imports = [
    ./minecraft.nix
  ];

  home.packages = with pkgs; [
    (retroarch.withCores (cores: with cores; [
      dolphin
      mupen64plus
      beetle-psx-hw
    ]))
  ];
}
