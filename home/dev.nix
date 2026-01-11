{
  pkgs,
  ...
}:
{
  # emacs my beloved
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30;
    extraPackages = epkgs: [
      epkgs.vterm
    ];
  };
  services.emacs = {
    enable = true;
  };
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
    nil
    # ocaml
    ocaml
    dune_3
    ocamlPackages.ocaml-lsp
    ocamlPackages.ocamlformat
    # rust
    rustup
    cargo-generate
    bacon
    # vterm
    cmake
    libtool
    libvterm
  ];

}
