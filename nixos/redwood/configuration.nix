# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  pkgs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common.nix
    ../stylix.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.configurationLimit = 50;

  nix.settings = {
    trusted-users = ["root" "sahana"];
    experimental-features = [
      "nix-command"
      "flakes"
    ];
  };

  # auto cleanup every week, keep last 2 weeks of generations
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 14d";
  };


  networking.hostName = "redwood"; # Define your hostname.

  networking.networkmanager.enable = true;
  time.timeZone = "America/New_York";
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };
  # unfree
  nixpkgs.config.allowUnfree = true;

  # NVIDIA settings
  services.xserver.videoDrivers = [ "nvidia" ];
  environment.systemPackages = with pkgs; [
    xwayland
    wayland-utils
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
    theme = "catppuccin-mocha";
    package = pkgs.kdePackages.sddm;
  };

  programs.light.enable = true;
  services.printing.enable = true;

  # audio & bluetooth
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.blueman.enable = true;

  services.libinput.enable = true;

  services.openssh.enable = true;

  # install flatpak
  services.flatpak.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
  users.users.sahana = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
      "docker"
      "video"
    ];
  };

  # Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    extraCompatPackages = [ pkgs.proton-ge-bin pkgs.gamescope ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
