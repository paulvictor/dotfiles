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
  vmLease = "${ipPrefix}.16";
  wgAddress = {
    host = "172.16.100.1";
    vm = "172.16.100.2";
  };
  hostWGConfig =
    ''
      [Interface]
      Address = ${wgAddress.host}/32
      PrivateKey = ${hostKeys.priv}
      ListenPort = ${wgPort}
      [Peer]
      PublicKey = ${vmKeys.pub}
      Endpoint = ${vmLease}:${wgPort}
      AllowedIPs = ${wgAddress.vm}/32
    '';
  tunDevice = "gptun";
in {
  inherit ipPrefix bridgeIp vmMac vmLease wgAddress tunDevice;
  host.wg = {config, lib, pkgs, ...}:
    {
      services.wireproxy = {
        enable = false;
        configFile = pkgs.writeText "wireproxy-conf" ''
          [Interface]
          Address = ${wgAddress.host}/32
          PrivateKey = ${hostKeys.priv}
          ListenPort = ${toString wgPort}
          [Peer]
          PublicKey = ${vmKeys.pub}
          Endpoint = ${vmLease}:${toString wgPort}
          AllowedIPs = ${wgAddress.vm}/32
          # Socks5 creates a socks5 proxy on your LAN, and all traffic would be routed via wireguard.
          [Socks5]
          BindAddress = 127.0.0.1:3128
          # http creates a http proxy on your LAN, and all traffic would be routed via wireguard.
          [http]
          BindAddress = 127.0.0.1:25345
        '';
      };
      sops.secrets."shared/wordpass" = {
        sopsFile = ../../../secrets/passwd.conf;
        format = "binary";
        mode = "0777";
        owner = "microvm";
        group = "kvm";
      };
      users.users.microvm.extraGroups = ["keys"];
      networking.wg-quick.interfaces.to-vm = {
        autostart = true;
        privateKey = hostKeys.priv;
        address = ["${wgAddress.host}/32"];
        listenPort = wgPort;
        peers = [{
          publicKey = vmKeys.pub;
          endpoint = "${vmLease}:${toString wgPort}";
          allowedIPs = ["${wgAddress.vm}/32"];
        }];
      };
    };
  vm.wg = {config, lib, pkgs, ...}:
    let
      sharedDir = "/shared";
    in {
      #      nix.settings.extra-sandbox-paths = [
      #         "/run/shared/wg"
      #       ];
      microvm.shares = [
        {
          tag = "wordpass";
          source = "/run/secrets/shared";
          mountPoint = sharedDir;
        } ];
      networking.openconnect.interfaces.${tunDevice}.passwordFile = "${sharedDir}/wordpass";

      networking.wg-quick.interfaces.to-host = {
        autostart = true;
        privateKey = vmKeys.priv;
        address = ["${wgAddress.vm}/32"];
        listenPort = wgPort;
        peers = [{
          publicKey = hostKeys.pub;
          endpoint = "${bridgeIp}:${toString wgPort}";
          allowedIPs = ["${wgAddress.host}/32"];
        }];
      };
    };
}
