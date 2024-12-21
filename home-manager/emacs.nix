{
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    # LSP servers
    emacs-lsp-booster
    ocamlPackages.merlin
  ];
}
