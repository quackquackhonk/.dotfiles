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
        libxtst
        libxt
      ];
     })

    # for MCSR
    piper
    waywall
  ];

}
