{
  pkgs,
  system,
  inputs,
  config,
  lib,
  myLib,
  ...
}: let
  cfg = config.myHomeManager;

  # Taking all modules in ./features and adding enables to them
  features =
    myLib.extendModules
    (name: {
      extraOptions = {
        myHomeManager.${name}.enable = lib.mkEnableOption "enable my ${name} configuration";
      };

      configExtension = config: (lib.mkIf cfg.${name}.enable config);
    })
    (myLib.filesIn ./features);
in {
  imports =
    []
    ++ features;
}
