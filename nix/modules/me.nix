# The primary user's identity; used by both NixOS and home-manager
{
  flake.modules.nixos.me = ./_me-options.nix;
  flake.modules.homeManager.me = ./_me-options.nix;
}
