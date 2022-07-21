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
      inputs.homeManager.packages."${system}".default
    ];
    nixpkgs = {
      inherit overlays system;
    };
  };
  machineSpecific = _: {
    networking.hostName = "crash";
    networking.computerName = "Crash";
  };

  setupNixPath = {config, lib, ...}: {
    environment.etc =
      mapAttrs'
        (name: value: { name = "nix/inputs/${name}"; value = { source = value.outPath; }; })
        inputs;
    nix.nixPath = [ "/etc/nix/inputs" ];
  };

in
darwin.lib.darwinSystem {
  inherit system;
  inputs = {
    inherit nixpkgs darwin;
  };
  modules = [
    setupNixPath
    commonModules
    machineSpecific
    ../services/sshd.nix
     # https://evilmartians.com/chronicles/stick-with-security-yubikey-ssh-gnupg-macos
    ../services/gpg-agent.nix
    ../services/ln-ssh-auth-sock.nix
    ../services/tuns.nix
    ../services/power-mgmt.nix
    ../modules/networking.nix
  ];
}
