{
  description = "its my emacs configuration!!";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Alejandra (nix formatter)
    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
      nixpkgs,
      home-manager,
      alejandra,
      ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    inherit (self) outputs;
  in {
    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#your-hostname'
    nixosConfigurations = {
      # laptop configuration
      monstera = nixpkgs.lib.nixosSystem rec {
        specialArgs = {inherit inputs outputs;};
        system = "x86_64-linux";
        modules = [
          ./nixos/monstera/configuration.nix
          {environment.systemPackages = [alejandra.defaultPackage.${system}];}
          inputs.flake-programs-sqlite.nixosModules.programs-sqlite
        ];
      };

      # Desktop configuratio
      redwood = nixpkgs.lib.nixosSystem rec {
        specialArgs = {inherit inputs outputs;};
        system = "x86_64-linux";
        modules = [
          ./nixos/redwood/configuration.nix
          {environment.systemPackages = [alejandra.defaultPackage.${system}];}

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
