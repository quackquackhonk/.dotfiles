{
  description = "Your new nix config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

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
    inherit (self) outputs;
  in {
    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#your-hostname'
    nixosConfigurations = {
      monstera = nixpkgs.lib.nixosSystem rec {
        specialArgs = {inherit inputs outputs;};
        system = "x86_64-linux";
        # > Our main nixos configuration file <
        modules = [
          {environment.systemPackages = [alejandra.defaultPackage.${system}];}
          {
            # pin system nixpkgs to the same version as the flake input
            # (don't see a way to declaratively set channels but this seems to work fine?)
            nix.nixPath = ["nixpkgs=${nixpkgs}"];
          }
          ./nixos/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.sahana = import ./home-manager/home.nix;
          }
        ];
      };
    };
  };
}
