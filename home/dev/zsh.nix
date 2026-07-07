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
    # use legacy zsh configuration location
    dotDir = "${config.xdg.configHome}/zsh";

    shellAliases = {
      l = "eza -la";
      ls = "eza";
      b = "bat";

    };

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
    };
  };
  # cli tools
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
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
