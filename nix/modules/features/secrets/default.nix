# Deployed secrets (sops-nix): extra nix.conf. Features that declare
# sops.secrets (syncthing, gp-vpn, remote-builds) rely on this feature being present.
{ inputs, ... }: {
  flake.modules.nixos.secrets = { config, ... }: {
    imports = [ inputs.sops-nix.nixosModules.sops ];

    sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    users.users.${config.me.username}.extraGroups = [ "keys" ]; # reads the decrypted secrets

    sops.secrets."extra-nix-conf" = {
      sopsFile = ../../../secrets/nix-conf-extra.conf;
      format = "binary";
      mode = "0440";
      group = config.users.groups.keys.name;
    };
    nix.extraOptions = ''
      !include ${config.sops.secrets.extra-nix-conf.path}
    '';
    nix.settings.extra-sandbox-paths = [
      "/run/secrets" # TODO make this part of activation
    ];
  };
}
