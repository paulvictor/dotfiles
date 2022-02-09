{
  description = "The meta config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, home-manager, nixpkgs, sops-nix }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        config.permittedInsecurePackages = [ "steghide-0.5.1" ];
#         overlays = [ tomb-overlay firejail-overlay guix-overlay ];
      };
      lib = nixpkgs.lib;
    in {
      nixosConfigurations = {
        uriel = lib.nixosSystem {
          inherit system;
          modules = [
            ./root/configuraton.nix
          ];
        };
      };
    }
