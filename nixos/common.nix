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
  ];

  fonts.packages = with pkgs; [
    nerd-fonts.iosevka
  ];
}
