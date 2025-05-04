{
  description = "My system confugiration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    # AGS bar for hyprland
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";
    hyprpanel.inputs.nixpkgs.follows = "nixpkgs";

    # stylix for theming
    stylix = {
      url = "github:danth/stylix";

      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      stylix,
      home-manager,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
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
              { environment.systemPackages = [ ]; }
            ];
          };

          # Desktop configuration
          redwood = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit inputs outputs; };
            modules = [
              { nixpkgs.overlays = [inputs.hyprpanel.overlay]; }
              ./nixos/redwood/configuration.nix
              home-manager.nixosModules.home-manager
              {
                home-manager.backupFileExtension = "hm-backup";
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.sahana.imports = [
                  inputs.hyprpanel.homeManagerModules.hyprpanel
                  ./home-manager/home.nix
                ];
              }
            ];
          };
        };
      };
}
