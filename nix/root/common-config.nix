{ config, pkgs, lib, ...}:

{
#   nix.nixPath = [
#     "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
#     #"nixpkgs-unstable=${fetchTarball "https://github.com/NixOS/nixpkgs/tarball/master"}"
#     #"nixos-config=/etc/nixos/configuration.nix"
#     "/nix/var/nix/profiles/per-user/root/channels" ];

  nix.autoOptimiseStore = true;
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    experimental-features = nix-command flakes
  '';
  nix.package = pkgs.nixFlakes;
  nix.systemFeatures = [ "kvm" "big-parallel" ];

  services.guix.enable = true;
}
