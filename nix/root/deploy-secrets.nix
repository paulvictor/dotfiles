{config, lib, ...}:

{
  imports = [
    ./deploy-secrets/nix-conf.nix
    ./deploy-secrets/tailscale.nix
    ./deploy-secrets/juspay-wg.nix
  ];
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
}
