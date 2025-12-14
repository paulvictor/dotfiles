{config, lib, ...}:
lib.mkIf (lib.pathExists ../../secrets/${config.networking.hostName}/tailscale.conf) {
  sops.secrets."tailscale.authkey" = {
    sopsFile = ../../secrets/${config.networking.hostName}/tailscale.conf;
    format = "binary";
    mode = "0440";
    group = config.users.groups.keys.name;
  };
}
