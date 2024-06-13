{ config, lib, pkgs, inputs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      inputs.home-manager.nixosModules.default
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  networking.hostName = "monstera";
  networking.networkmanager.enable = true;
  time.timeZone = "US/Eastern";
  i18n.defaultLocale = "en_US.UTF-8";

  programs.hyprland.enable = true;
  programs.light.enable = true;

  services.printing.enable = true;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };
  services.libinput.enable = true;
  services.openssh.enable = true;
  # networking.firewall.allowedTCPPorts = [ 22 ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  programs.fish.enable = true;
  users.defaultUserShell = pkgs.fish;
  users.users.sahana = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ]; # Enable ‘sudo’ for the user.
  };

  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    users = {
      "sahana" = import ./home.nix;
    };
  };

  # Enable dynamic libraries
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    gcc
    gnumake
    gnupg
    wget
    curl
  ];
  
  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep-since 4d --keep 3";
    flake = "/home/sahana/nixos";
  };

  # Never ever change this
  system.stateVersion = "24.05"; # Did you read the comment?

}

