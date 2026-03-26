# Refer https://github.com/nix-community/home-manager/issues/3075#issuecomment-3037360368
{withSystem, inputs, self, ...}:
{
  perSystem = {pkgs, ...}: {
    legacyPackages.homeConfigurations =
      import ../userland/default.nix { inherit inputs pkgs; };
  };
}
