{
  pkgs,
  ...
}:
{
  imports = [
    ./minecraft.nix
  ];

  home.packages = with pkgs; [
    # for window compat
    wine
    winetricks
    # for modding some steam games (ETG)
    r2modman

    # FOSS epic games
    heroic

    # retroarch
    (retroarch.withCores (cores: with cores; [
      dolphin
      mupen64plus
      beetle-psx-hw
    ]))
  ];
}
