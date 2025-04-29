# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  lib,
  config,
  pkgs,
  ...
}:
{
  # you can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule
    ./zsh.nix
    ./dev.nix
  ];
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  services.mpris-proxy.enable = true;

  home.username = "sahana";
  home.homeDirectory = "/home/sahana";
  home.stateVersion = "24.11"; # Please read the comment before changing.

  programs.git = {
    enable = true;
    userName = "Sahana Tankala";
    userEmail = "sahanatankala@gmail.com";
  };

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # utils
    pkg-config
    lshw
    eza
    bat
    fd
    ripgrep
    jq
    yq-go
    zip
    xz
    unzip
    p7zip
    file
    which
    tree
    gnused
    gnutar
    gawk
    zstd
    gnupg
    btop # replacement of htop/nmon
    lsof # list open files

    # networking tools
    mtr
    dnsutils
    nmap
    ipcalc

    # system tools
    sysstat
    lm_sensors # for `sensors` command
    ethtool
    pciutils # lspci
    usbutils # lsusb

    # hyprland utils
    nwg-look
    xdg-desktop-portal-hyprland
    hyprpolkitagent
    waybar
    # hyprpanel
    tofi
    dunst
    hyprpaper
    hyprcursor
    hyprlock
    wlogout
    udiskie
    pavucontrol

    # apps
    kdePackages.dolphin
    kdePackages.qtwayland
    kdePackages.qtsvg
    kdePackages.ark
    prismlauncher
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Let home-manager manage my dotfiles
  home.file = {
    ".config/starship".source = ../starship/.config/starship.toml;
    ".config/hypr" = {
      source = ../hypr;
      recursive = true;
    };
    ".config/wlogout" = {
      source = ../wlogout;
      recursive = true;
    };
    ".config/dunst/dunstrc".source = ../dunst/dunstrc;
    ".config/ghostty" = {
      source = ../ghostty;
      recursive = true;
    };
    ".config/waybar" = {
      source = ../waybar;
      recursive = true;
    };
    ".config/tofi/config".text = ''
      # Catppuccin Mocha
      width = 100%
      height = 100%
      border-width = 0
      outline-width = 0
      padding-left = 35%
      padding-top = 35%
      result-spacing = 25
      num-results = 5
      font = Iosevka Nerd Font
      text-color = #cdd6f4
      prompt-color = #f38ba8
      selection-color = #f9e2af
      background-color = #000A
    '';
  };
}
