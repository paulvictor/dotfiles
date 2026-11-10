# Keyboard driven pointer control
{
  flake.overlays.warpd = import ./_overlay.nix;
  flake.modules.homeManager.warpd = ./_config.nix;
}
