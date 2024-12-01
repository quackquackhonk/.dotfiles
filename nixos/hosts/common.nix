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
  ];

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;

    shellAliases = {
      l = "eza -la";
      ls = "eza";

      g = "git";
      gs = "git status";
      ga = "git add";
      gc = "git commit";

      update = "nh os switch /home/sahana/dotfiles/nixos/.";
    };

    ohMyZsh = {
      enable = true;
      plugins = [ "git" ];
    };

  };


  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep-since 4d --keep 3";
  };
}
