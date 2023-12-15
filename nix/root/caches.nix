{ config, lib, pkgs, ... }:

{
  nix.settings.substituters = [
    "https://nix-community.cachix.org"
    "https://miso-haskell.cachix.org"
  ];
  nix.settings.trusted-substituters = [
    "https://nix-community.cachix.org"
    "https://miso-haskell.cachix.org"
  ];
  nix.settings.trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
  ];
}
