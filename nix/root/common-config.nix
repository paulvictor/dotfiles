{ config, pkgs, lib, specialArgs, ...}:

{
  nix.autoOptimiseStore = true;
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    experimental-features = nix-command flakes
  '';
  nix.package = pkgs.nixFlakes;
  nix.systemFeatures = [ "kvm" "big-parallel" ];

  boot.binfmt.emulatedSystems =
    lib.optionals
      specialArgs.isPhysicalDevice
        [ "aarch64-linux" "armv7l-linux" "riscv64-linux" ];

  services.guix.enable = true;
}
