{
  description = "My system confugiration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";

    # Home manager for managing the home dir
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # some helpful utils
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    # zen-browser
    zen-browser.url = "github:0xc000022070/zen-browser-flake";

    # stylix for theming
    stylix = {
      url = "github:danth/stylix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    # caelestia for shell / bar
    caelestia-shell = {
      url = "github:caelestia-dots/shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs =
    {
      self,
      nixpkgs,
      stylix,
      zen-browser,
      home-manager,
      caelestia-shell,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      inherit (self) outputs;
    in
      {
        # NixOS configuration entrypoint
        # Available through 'nixos-rebuild --flake .#your-hostname'
        nixosConfigurations = {
          # laptop configuration
          monstera = nixpkgs.lib.nixosSystem {
            specialArgs = { inherit inputs outputs; };
            modules = [
              inputs.flake-programs-sqlite.nixosModules.programs-sqlite
              ./nixos/monstera/configuration.nix
              { environment.systemPackages = []; }
            ];
          };

          # Desktop configuration
          redwood = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit inputs outputs; };
            modules = [
              ./nixos/redwood/configuration.nix
              home-manager.nixosModules.home-manager
              {
                home-manager.backupFileExtension = "backup2";
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.extraSpecialArgs = { inherit inputs system; };
                home-manager.users.sahana.imports = [
                  stylix.homeModules.stylix
                  ./home/home.nix
                ];
              }
            ];
          };
        };
      };
}
