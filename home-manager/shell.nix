{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;

    shellAliases = {
      l = "eza -la";
      ls = "eza";

      update = "cd ~/dotfiles && git add . && sudo nixos-rebuild switch --flake /home/sahana/dotfiles/. && hyprctl reload && cd -";
    };

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
    };

  };
  programs.mise = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
}
