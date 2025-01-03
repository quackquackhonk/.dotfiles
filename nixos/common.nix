{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  # Enable dynamic libraries
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    curl
    emacs30
    git
    gcc
    gnumake
    gnupg
    networkmanagerapplet
    nixfmt-rfc-style
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



  fonts.packages = with pkgs; [
    nerd-fonts.iosevka
  ];

  # nix helper
  programs.nh = {
    enable = true;
    clean = {
      enable = true;
      extraArgs = "--keep-since 4d --keep 3";
    };
    flake = "/home/sahana/dotfiles";
  };
}
