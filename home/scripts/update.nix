{
  pkgs,
  config,
  ...
}:
{
  home.packages = [
    # Update the system
    (pkgs.writeShellApplication {
      name = "update";
      # Dependencies automatically added to PATH
      # runtimeInputs = [];
      text = ''
cd ${config.home.homeDirectory}/dotfiles

git add .

sudo nixos-rebuild switch --flake .

cd -
    '';
    })

    # Reload running processes after an update
    (pkgs.writeShellApplication {
      name = "reload";
      # Dependencies automatically added to PATH
      # runtimeInputs = [ pkgs.stow ];
      text = ''
#shellcheck disable=SC1091
hyprctl reload;
dms restart;
    '';
    })
  ];


}
