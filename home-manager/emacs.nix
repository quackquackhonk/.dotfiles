{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    # LSP servers
    emacs-lsp-booster
    ocamlPackages.merlin

    # nix
    nixfmt-rfc-style

    # vterm
    cmake
    libtool
    libvterm
  ];
}
