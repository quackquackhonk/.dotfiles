{
  inputs,
  system,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    # launcer
    (prismlauncher.override {
      additionalLibs = [
        libxkbcommon
        xorg.libXtst
        xorg.libXt
      ];
     })

    # for MCSR
    piper
    waywall
  ];

}
