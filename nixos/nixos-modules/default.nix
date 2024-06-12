{
  pkgs,
  config,
  lib,
  inputs,
  outputs,
  utils,
  ...
}: let
  cfg = config.myNixOS;

  # Taking all modules in ./features and adding enables to them
  features =
    utils.extendModules
    (name: {
      extraOptions = {
        myNixOS.${name}.enable = lib.mkEnableOption "enable my ${name} configuration";
      };

      configExtension = config: (lib.mkIf cfg.${name}.enable config);
    })
    (utils.filesIn ./features);

  # Taking all module services in ./services and adding services.enables to them
  # services =
  #   utils.extendModules
  #   (name: {
  #     extraOptions = {
  #       myNixOS.services.${name}.enable = lib.mkEnableOption "enable ${name} service";
  #     };
  #
  #     configExtension = config: (lib.mkIf cfg.services.${name}.enable config);
  #   })
  #   (utils.filesIn ./services);
in {
  imports =
    [
      inputs.home-manager.nixosModules.home-manager
    ]
    ++ features;

  options.myNixOS = {
    sharedSettings = {
      hyprland.enable = lib.mkEnableOption "enable hyprland";
    };
  };

  config = {
    nix.settings.experimental-features = ["nix-command" "flakes"];
    programs.nix-ld.enable = true;
    nixpkgs.config.allowUnfree = true;
  };
}
