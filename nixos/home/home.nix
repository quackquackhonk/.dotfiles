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

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
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
    # archives
    zip
    xz
    unzip
    p7zip

    # utils
    stow
    starship
    fzf
    zoxide
    eza
    bat
    fd
    jq
    yq-go

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

    # desktop utils
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

  # HYPRLAND
  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";
    "$browser" = "firefox";
    "$term" = "kitty";
    bind = [
      "$mod, Return, exec, $term"
      "$mod, B, exec, $browser"
      "$mod, E, exec, emacsclient -a =''"

    ] ++ (
      # workspaces:
      builtins.concatLists (builtins.genList (i:
        let ws = i + 1;
        in [
          # bind ~$mod NUM~ to goto workspace NUM
          "$mod, code:1${toString i}, workspace, ${toString ws}"
          # bind ~$mod SHIFT NUM~ to move window to workspace NUM
          "$mod, code:1${toString i}, movetoworkspace, ${toString ws}"
        ]
      ))
    );
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
