{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    # LSP servers
    emacs-lsp-booster

    # nix
    nixfmt-rfc-style

    # vterm
    cmake
    libtool
    libvterm

    # ocaml
    ocaml
    dune_3
    ocamlPackages.ocaml-lsp
    ocamlPackages.ocamlformat
  ];

}
