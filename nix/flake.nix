{
  description = "Meta Config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    homeManager.url = "github:nix-community/home-manager";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    emacsOverlay.url = "github:nix-community/emacs-overlay";
    emacsOverlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, homeManager, nixpkgs, sops-nix, emacsOverlay }:
    let
      system = "x86_64-linux";
      lib = nixpkgs.lib;
    in
      {
        nixosConfigurations =
          import ./root/devices/default.nix { inherit lib nixpkgs system self homeManager sops-nix; };
        homeConfiguration = {
          viktor = {};
        };
      };

}
