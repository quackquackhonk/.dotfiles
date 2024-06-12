{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    ags.url = "github:Aylur/ags";
  };

  outputs = { ... }@inputs: let 
    utils = import ./utils/default.nix { inherit inputs; };
  in with utils; {
    nixosConfigurations = {
      monstera = mkSystem ./hosts/monstera/configuration.nix;
    };

    homeConfigurations = {
      "sahana@monstera" = mkHome "x86_64-linux" ./hosts/monstera/home.nix;
    };
    
    homeManagerModules.default = ./hm-modules;
    nixosModules.default = ./nixos-modules;
  };
}
