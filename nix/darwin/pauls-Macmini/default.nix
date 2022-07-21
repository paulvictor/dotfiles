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

    environment.shells = [ pkgs.bashInteractive_5 pkgs.zsh ];
    environment.systemPath = [ "/run/current-system/sw/bin" ];

    programs.bash = {
      enable = true;
    };
  };
  machineSpecific = _: {
    networking.hostName = "crash";
    networking.computerName = "Crash";
  };

  setupNixPath = {config, lib, ...}: {
    nix.nixPath =
      lib.mapAttrs'
        (name: value: { inherit name; value = value.outPath; })
        inputs;
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
