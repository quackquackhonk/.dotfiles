# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  system,
  pkgs,
  ...
}:
{
  # you can import other home-manager modules here
  imports = [
    inputs.zen-browser.homeModules.beta
    ./zsh.nix
    ./dev.nix
    ./stylix.nix
    ./hyprland.nix
    ./hyprpanel.nix
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

  programs.zen-browser = {
    enable = true;
    policies.DisableTelemetry = true;
    nativeMessagingHosts = [pkgs.firefoxpwa];
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

    # apps
    feh
    zathura
    prismlauncher
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  xdg = {
    enable = true;
    portal = {
      enable = true;
    };
  };

  # Let home-manager manage my dotfiles
  home.file = {
    ".config/starship".source = ../starship/.config/starship.toml;
    ".config/ghostty" = {
      source = ../ghostty;
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
      text-color = #cdd6f4
      prompt-color = #f38ba8
      selection-color = #f9e2af
      background-color = #000A
    '';
  };
}
