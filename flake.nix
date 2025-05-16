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

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    # AGS bar for hyprland
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";
    hyprpanel.inputs.nixpkgs.follows = "nixpkgs";

    # zen-browser
    zen-browser.url = "github:0xc000022070/zen-browser-flake";

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
      zen-browser,
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
                home-manager.extraSpecialArgs = { inherit inputs system; };
                home-manager.users.sahana.imports = [
                  inputs.hyprpanel.homeManagerModules.hyprpanel
                  ./home/home.nix
                ];
              }
            ];
          };
        };
      };
}
