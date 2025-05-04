{
  pkgs,
  ...
}:
{
home.file = {
    ".emacs.d/init.el".source = ../emacs/.emacs.d/init.el;
    ".emacs.d/early-init.el".source = ../emacs/.emacs.d/early-init.el;
};
  home.packages = with pkgs; [
    # LSP servers
    emacs-lsp-booster

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
