{
  pkgs,
  ...
}:
{
  home.file = {
    ".emacs".source = ../emacs/.emacs;
  };
  home.packages = with pkgs; [
    # LSP servers
    emacs-lsp-booster
    python3
    renpy

    # nix
    nixfmt-rfc-style
    nil

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
