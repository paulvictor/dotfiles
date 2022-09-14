{ pkgs, specialArgs, lib, ...}:

{
  environment.systemPackages =
    lib.optionals (specialArgs.isPhysicalDevice) [
      specialArgs.kmonad.packages.x86_64-linux.kmonad
    ];
}
