{
  config,
  lib,
  ...
}: {
  imports = [
    ./stylix.nix
  ];

  config.var = {
    autoUpgrade = false;
    autoGarbageCollector = true;
  };

  # DON'T TOUCH THIS
  options = {
    var = lib.mkOption {
      type = lib.types.attrs;
      default = {};
    };
  };
}
