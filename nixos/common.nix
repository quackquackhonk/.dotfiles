{
  pkgs,
  ...
}: {
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
        font  = "Noto Sans";
        fontSize = "9";
        background = "${../wallpapers/space.png}";
        loginBackground = true;
      }
    )
  ];

  # manage removeable media
  services.udisks2.enable = true;

  fonts.packages = with pkgs; [
    nerd-fonts.iosevka
  ];
}
