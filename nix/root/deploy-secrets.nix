{config, lib, ...}:

{
  imports = [
    ./deploy-secrets/nix-conf.nix
    ./deploy-secrets/tailscale.nix
    ./deploy-secrets/juspay-wg.nix
  ];
}
