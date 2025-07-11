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
    # python
    python3
    # gleam
    gleam
    erlang
    rebar3
    # nix
    nixfmt-rfc-style
    nil
    # ocaml
    ocaml
    dune_3
    ocamlPackages.ocaml-lsp
    ocamlPackages.ocamlformat
    # vterm
    cmake
    libtool
    libvterm

  ];

}
