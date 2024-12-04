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

      g = "git";
      gs = "git status";
      ga = "git add";
      gc = "git commit";

      update = "sudo nixos-rebuild switch --flake /home/sahana/dotfiles/.";
    };

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
    };

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
