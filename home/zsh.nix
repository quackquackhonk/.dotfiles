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
      b = "bat";

      update = "cd ~/dotfiles && git add . && sudo nixos-rebuild switch --flake . && hyprctl reload && cd -";
    };

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
      extraConfig = ''
function qqh_flake_template() {
    nix flake init -t github:nix-community/templates#"$1"
}
'';
    };
  };
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
