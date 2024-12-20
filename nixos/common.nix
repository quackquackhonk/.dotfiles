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
    networkmanagerapplet
    kitty
    git
    gcc
    gnumake
    gnupg
    wget

    # catppuccin sddm overlay
    (
      catppuccin-sddm.override {
        flavor = "mocha";
        font  = "Noto Sans";
        fontSize = "11";
        background = "${../wallpapers/4k_pixel_at.png}";
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
