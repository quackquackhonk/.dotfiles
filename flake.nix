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
    # hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      # pkgs = import nixpkgs {
      #   inherit system;
      #   overlays = [ inputs.hyprpanel.overlay ];
      # };
      # pkgs = nixpkgs.legacyPackages.${system}.extend inputs.hyprpanel.overlay;
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
            ./nixos/monstera/configuration.nix
            { environment.systemPackages = [ ]; }
            inputs.flake-programs-sqlite.nixosModules.programs-sqlite
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
              home-manager.backupFileExtension = "backup";
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.sahana = import ./home-manager/home.nix;
            }
          ];
        };
      };
    };
}
