args@{ nixpkgs, self, lib, inputs, overlays, ...}:

let
  system = flake-utils.lib.system.aarch64-darwin;
  inherit (inputs) darwin  flake-utils;
  commonModules = {pkgs,...}: {
    system.stateVersion = 6;
    nix.package = pkgs.nixStable;
    nix.extraOptions = ''
      experimental-features = nix-command flakes
      gc-keep-outputs = true
      gc-keep-derivations = true
      build-users-group = nixbld
    '';
    environment.systemPackages = with pkgs;[
      inputs.homeManager.packages."${system}".default
    ];
    nixpkgs = {inherit overlays system;};

    environment.shells = [ pkgs.bashInteractive pkgs.zsh ];
    environment.systemPath = [ "/run/current-system/sw/bin" ];

    programs.bash.enable = true;
    documentation.enable = true;
    documentation.man.enable = true;
    documentation.doc.enable = true;
    documentation.info.enable = true;
    networking = {dns = [ "8.8.8.8" "8.8.4.4" ];};
  };

  machineSpecific = {
    networking.hostName = "crash";
    networking.computerName = "Crash";
    networking.knownNetworkServices = ["Wi-Fi" "Ethernet Adaptor" "Thunderbolt Ethernet"];
  };

in
darwin.lib.darwinSystem {
  inherit system;
  inputs = {
    inherit nixpkgs darwin;
  };
  specialArgs = {
    userName = "paul.victor";
  };
  modules = [
    inputs.homeManager.darwinModules.default
    commonModules
    machineSpecific
    ../services/sshd.nix
    ../services/admin.nix

     # https://evilmartians.com/chronicles/stick-with-security-yubikey-ssh-gnupg-macos
#     ../services/gpg-agent.nix
#     ../services/ln-ssh-auth-sock.nix
#     ../services/tuns.nix
    ../services/power-mgmt.nix
    ../modules/networking.nix
  ];
}
