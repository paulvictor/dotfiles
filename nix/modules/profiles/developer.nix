# Development tools: shell, editor, git and language tooling, pinned nixpkgs, signing keys, cross-arch builds,
# containers and VMs
{ config, ... }: {
  profiles.developer = {
    nixos = with config.features.nixos; [ dev nix-dev emulation gpg-yubikey virtualisation ];
    home = with config.features.home; [ shell ssh dev emacs pass schemesh gpg-yubikey ];
  };
}
