{
  description = "The meta config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, home-manager, nixpkgs, sops-nix }: {

    let
      system = "x86_64-linux";
      gllock = pkgs.callPackage ./common/pkgs/gllock.nix {};
      tomb-overlay = import ./common/pkgs/tomb.nix;
      firejail-overlay = import ./common/pkgs/firejail.nix;
      xkeyboardconfig-overlay =
        self: super:
          { xkeyboard_config =
              super.xkeyboard_config.overrideDerivation(oldAttrs: {
                src =
                  fetchTarball
                    https://www.x.org/releases/individual/data/xkeyboard-config/xkeyboard-config-2.28.tar.gz; }); };
      guix-overlay = self: super: { guix = pkgs.callPackage ./packages/guix.nix {}; };
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        config.permittedInsecurePackages = [ "steghide-0.5.1" ];
        overlays = [ tomb-overlay firejail-overlay guix-overlay ];
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
    }

  };
}
