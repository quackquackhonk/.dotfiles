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
  ];

}
