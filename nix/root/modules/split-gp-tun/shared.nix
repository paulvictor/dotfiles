let
  wgPort = 51820;
  # TODO generate these as part of activationScripts
  hostKeys = {
    priv = "MJWd2oYFwGv4K2F95ExVCyRjA64wUwbemhVORWoCc20="; # "/run/shared/wg/host/priv";
    pub = "nXoiZoHgKhbCxdXOlLCZyT/7FHnBzzQH1sa4rbLYVnY="; # "/run/shared/wg/host/pub";
  };
  vmKeys = {
    priv = "qCcWcWLvQnxhUiqDifeESknESRoUny1MrIvM5vnIOUY="; # "/run/shared/wg/vm/priv";
    pub = "RVivXjDvKZPyZXnvVq+5s+lRWqQg/zcX/+ZUnSgOygs=" ; # "/run/shared/wg/vm/pub";
  };
  ipPrefix = "172.16.55";
  bridgeIp = "${ipPrefix}.1";
  vmMac = "02:00:00:00:00:01"; # How do i make this unique?
  vmLease = "${ipPrefix}.18";
  wgAddress = {
    host =  "172.16.100.1/32";
    vm =  "172.16.100.2/32";
  };
  hostWGConfig =
    ''
      [Interface]
      Address = ${wgAddress.host}
      PrivateKey = ${hostKeys.priv}
      ListenPort = ${wgPort}
      [Peer]
      PublicKey = ${vmKeys.pub}
      Endpoint = ${vmLease}:${wgPort}
      AllowedIPs = ${wgAddress.vm}
    '';
in {
  inherit ipPrefix bridgeIp vmMac vmLease;
  host.wg = {config, lib, pkgs, ...}:
    {
      services.wireproxy = {
        enable = true;
        configFile = pkgs.writeText "wireproxy-conf" ''
          [Interface]
          Address = ${wgAddress.host}
          PrivateKey = ${hostKeys.priv}
          ListenPort = ${toString wgPort}
          [Peer]
          PublicKey = ${vmKeys.pub}
          Endpoint = ${vmLease}:${toString wgPort}
          AllowedIPs = ${wgAddress.vm}
          # Socks5 creates a socks5 proxy on your LAN, and all traffic would be routed via wireguard.
          [Socks5]
          BindAddress = 127.0.0.1:3128
          # http creates a http proxy on your LAN, and all traffic would be routed via wireguard.
          [http]
          BindAddress = 127.0.0.1:25345
        '';
      };
      # nix.settings.extra-sandbox-paths = [
      #         "/run/shared/wg"
      #       ];
      networking.wg-quick.interfaces.to-vm = {
        autostart = false;
        privateKey = hostKeys.priv;
        address = [wgAddress.host];
        listenPort = wgPort;
        peers = [{
          publicKey = vmKeys.pub;
          endpoint = "${vmLease}:${toString wgPort}";
          allowedIPs = [wgAddress.vm];
        }];
      };
    };
  vm.wg = {config, lib, pkgs, ...}:
    {
      #      nix.settings.extra-sandbox-paths = [
      #         "/run/shared/wg"
      #       ];
      networking.wg-quick.interfaces.to-host = {
        autostart = true;
        privateKey = vmKeys.priv;
        address = [wgAddress.vm];
        listenPort = wgPort;
        peers = [{
          publicKey = hostKeys.pub;
          endpoint = "${bridgeIp}:${toString wgPort}";
          allowedIPs = [wgAddress.host];
        }];
      };
    };
}
