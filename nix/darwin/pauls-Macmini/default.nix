args@{ nixpkgs, self, pkgsFor, lib, inputs, overlays, ...}:

let
  system = flake-utils.lib.system.aarch64-darwin;
  inherit (inputs) darwin  flake-utils;
  commonModules = {pkgs,...}: {
    services.nix-daemon.enable = true;
    nix.package = pkgs.nix;
    nix.extraOptions = ''
      experimental-features = nix-command flakes
      gc-keep-outputs = true
      gc-keep-derivations = true
      build-users-group = nixbld
    '';
    environment.systemPackages = with pkgs;[
      #neovim
      openssh
      gnupg
      inputs.homeManager.packages."${system}".default
    ];
    nixpkgs = {
      inherit overlays system;
    };
  };
in
darwin.lib.darwinSystem {
  inherit system;
  inputs = {
    inherit nixpkgs darwin;
  };
  modules = [
    commonModules
    ../services/sshd.nix
     # https://evilmartians.com/chronicles/stick-with-security-yubikey-ssh-gnupg-macos
    ../services/gpg-agent.nix
    ../services/ln-ssh-auth-sock.nix
    ../services/tuns.nix
    ../services/power-mgmt.nix
  ];
}
