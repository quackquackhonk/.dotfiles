
{
  config,
  lib,
  pkgs,
  callPackage,
  inputs,
  ...
}:
let
  comic-code-font = pkgs.callPackage ./packages/comic-code-font.nix { inherit pkgs; };
in
{

  imports = [
    ./udev
  ];
  # Enable dynamic libraries
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [ icu ];

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

    (pkgs.catppuccin-sddm.override {
      flavor = "mocha";
      accent = "mauve";
      font = "Maple Mono";
      fontSize = "12";
      background = "${../wallpapers/space.png}";
      loginBackground = true;
    })

  ];

  # hyprland + SDDM
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };
  services.xserver.enable = true;
  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
    theme = "catppuccin-mocha-mauve";
  };

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
  programs.xfconf.enable = true; # save preferences
  services.gvfs.enable = true; # mounting / trash / etc
  services.tumbler.enable = true; # thumbnails for images

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
