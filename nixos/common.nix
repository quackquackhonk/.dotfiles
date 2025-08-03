{
  pkgs,
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
    emacs30
    git
    gcc
    gnumake
    gnupg
    networkmanagerapplet
    nixfmt-rfc-style
    ghostty
    kitty
    wget
    (
      pkgs.catppuccin-sddm.override {
        flavor = "mocha";
        font  = "Iosevka";
        fontSize = "12";
        background = "${../wallpapers/space.png}";
        loginBackground = true;
      }
    )
  ];

  # file manager
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [
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
      aporetic
      comic-code-font
    ];
  };
}
