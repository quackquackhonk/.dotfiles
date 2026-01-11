{
  config,
  pkgs,
  callPackage,
  ...
}:
let
  comic-code-font = pkgs.callPackage ./packages/comic-code-font.nix { inherit pkgs; };
in
{
  # Enable dynamic libraries
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    curl
    vim
    git
    gcc
    gnumake
    gnupg
    networkmanagerapplet
    nixfmt
    ghostty
    wget
    qmk
    via
    (
      pkgs.catppuccin-sddm.override {
        flavor = "mocha";
        font  = "Maple Mono";
        fontSize = "12";
        background = "${../wallpapers/space.png}";
        loginBackground = true;
      }
    )
  ];

  # for qmk
  hardware.keyboard.qmk.enable = true;
  services.udev.packages = [ pkgs.via ];


  # file manager
  programs.thunar = {
    enable = true;
    plugins = with pkgs; [
      thunar-volman
      thunar-archive-plugin
    ];
  };
  # needed for xfce apps
  programs.xfconf.enable = true;       # save preferences
  services.gvfs.enable = true;         # mounting / trash / etc
  services.tumbler.enable = true;      # thumbnails for images

  # manage removeable media
  services.udisks2.enable = true;

  # Fonts
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      iosevka
      maple-mono.truetype
      maple-mono.NF-unhinted
      comic-code-font
    ];
  };
}
