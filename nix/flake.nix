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
      gllock-overlay = import ./common/pkgs/gllock.nix;
      tomb-overlay = import ./common/pkgs/tomb.nix;
      firejail-overlay = import ./common/pkgs/firejail.nix;
#       xkeyboardconfig-overlay =
#         self: super:
#           { xkeyboard_config =
#               super.xkeyboard_config.overrideDerivation(oldAttrs: {
#                 src =
#                   fetchTarball
#                     https://www.x.org/releases/individual/data/xkeyboard-config/xkeyboard-config-2.28.tar.gz; }); };
      guix-overlay = import ./root/packages/guix.nix;
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        config.permittedInsecurePackages = [ "steghide-0.5.1" ];
        overlays = [ gllock-overlay tomb-overlay firejail-overlay guix-overlay ];
      };
      lib = nixpkgs.lib;
    in
      {
         nixosConfigurations = import ./root/devices/default.nix { inherit lib pkgs system self homeManager sops-nix; };

#         nixosConfigurations = {
#           uriel = lib.nixosSystem {
#             inherit system ;
#             inherit pkgs ;
#             modules = [
#               ./root/guix/modules/services/guix.nix
#               sops-nix.nixosModule
#               homeManager.nixosModule
#               ./root/common-config.nix
#               ./root/devices/uriel/default.nix
#             ];
#           };
#         };
      };
}
