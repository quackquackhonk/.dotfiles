{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    (prismlauncher.override {
      additionalLibs = [
        xorg.libXtst
        xorg.libXt
        libxkbcommon
      ];
     })

    (retroarch.withCores (cores: with cores; [
      dolphin
      mupen64plus
      beetle-psx-hw
    ]))
  ];
}
