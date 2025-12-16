{config, ...}:
{
  sops.secrets."extra-nix-conf" = {
    sopsFile = ../../secrets/nix-conf-extra.conf;
    format = "binary";
    mode = "0440";
    group = config.users.groups.keys.name;
  };

  nix.extraOptions = ''
    !include ${config.sops.secrets.extra-nix-conf.path}
  '';
  nix.settings.extra-sandbox-paths = [
    "/run/secrets"
  ];
}
