# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # you can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule
    ./shell.nix
  ];

  nixpkgs = {
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

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
    eza
    bat
    fd
    ripgrep
    jq
    yq-go
    # archives
    zip
    xz
    unzip
    p7zip

    # networking tools
    mtr
    dnsutils
    nmap
    ipcalc

    # misc
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

    # system tools
    sysstat
    lm_sensors # for `sensors` command
    ethtool
    pciutils # lspci
    usbutils # lsusb

    # languages
    rustup
    luajit

    # hyprland utils
    xdg-desktop-portal-hyprland
    hyprpolkitagent
    waybar
    tofi
    dunst
    hyprpaper
    hyprlock
    wlogout

    # apps
    firefox
    alacritty
    zathura
    imv
  ];


  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  services.mpris-proxy.enable = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Let home-manager manage my dotfiles
  home.file = {
    ".config/starship".source = ../starship/.config/starship.toml;
    ".config/hypr" = {
      source = ../hypr;
      recursive = true;
    };
    ".config/tofi/config".source = ../sources/catppuccin/tofi/themes/catppuccin-mocha;
  };
}
