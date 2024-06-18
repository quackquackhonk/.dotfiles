{
  description = "Your new nix config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Alejandra (nix formatter)
    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";

    # generate programs.sqlite for command-not-found
    flake-programs-sqlite.url = "github:wamserma/flake-programs-sqlite";
    flake-programs-sqlite.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    nixos-wsl,
    home-manager,
    alejandra,
    ...
  } @ inputs: let
    inherit (self) outputs;
  in {
    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#your-hostname'
    nixosConfigurations = {
      monstera = nixpkgs.lib.nixosSystem rec {
        specialArgs = {inherit inputs outputs;};
        system = "x86_64-linux";
        modules = [
          ./hosts/monstera/configuration.nix
          {environment.systemPackages = [alejandra.defaultPackage.${system}];}
          inputs.flake-programs-sqlite.nixosModules.programs-sqlite
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.sahana = import ./home/home.nix;
          }
        ];
      };
      redwood = nixpkgs.lib.nixosSystem rec {
        specialArgs = {inherit inputs outputs;};
        system = "x86_64-linux";
        modules = [
          nixos-wsl.nixosModules.default
          {
            system.stateVersion = "23.11";
            wsl.enable = true;
          }
          ./hosts/redwood/configuration.nix
          {environment.systemPackages = [alejandra.defaultPackage.${system}];}
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.sahana = import ./home/home.nix;
          }
        ];
      };
    };
  };
}
