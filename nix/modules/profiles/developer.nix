# Development tools: shell, editor, git and language tooling, signing keys, cross-arch builds, containers and VMs
{ config, ... }: {
  profiles.developer = {
    nixos = with config.features.nixos; [ nix-dev emulation gpg-yubikey virtualisation ];
    home = with config.features.home; [ shell ssh dev emacs pass schemesh gpg-yubikey ];
  };
}
