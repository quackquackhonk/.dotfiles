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
    emacs30
    kitty
    git
    gcc
    gnumake
    gnupg
    wget
    curl
    zsh
  ];

  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep-since 4d --keep 3";
  };

  services.emacs.enable = true;
  services.emacs.defaultEditor = true;
}
