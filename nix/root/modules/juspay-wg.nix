{config, ...}:

{
  systemd.network = {
    enable = true;
    networks."50-juspay-wg" = {
      matchConfig.Name = "wg0";
      address = [
        "10.8.0.8/24"
        "fdcc:ad94:bacf:61a4::cafe:8/112"
      ];
    };
    netdevs."50-juspay-wg" = {
      netdevConfig = {
        Name = "wg0";
        Kind = "wireguard";
      };
      wireguardConfig = {
        ListenPort = 51820;
        PrivateKeyFile = config.sops.secrets."juspay-wg/privateKey".path;
        RouteTable = "main";
      };
      wireguardPeers = [
        {
          AllowedIPs = [
            "10.10.0.0/18" "10.10.64.0/22" "10.10.68.0/26" "10.10.68.64/27" "10.10.68.96/30" "10.10.68.100/32" "10.10.68.102/31" "10.10.68.104/29" "10.10.68.112/32" "10.10.68.117/32" "10.10.68.118/31" "10.10.68.120/29" "10.10.68.128/26" "10.10.68.192/28" "10.10.68.208/29" "10.10.68.217/32" "10.10.68.218/31" "10.10.68.220/30" "10.10.68.224/27" "10.10.69.0/25" "10.10.69.128/27" "10.10.69.160/29" "10.10.69.168/31" "10.10.69.172/30" "10.10.69.176/28" "10.10.69.192/26" "10.10.70.0/24" "10.10.71.0/26" "10.10.71.64/32" "10.10.71.66/31" "10.10.71.68/30" "10.10.71.72/29" "10.10.71.80/28" "10.10.71.96/27" "10.10.71.128/25" "10.10.72.0/21" "10.10.80.0/20" "10.10.96.0/19" "10.10.128.0/17"];
          PublicKey = "9lufciYc4vE8hLBc3ybnRf2YZ+6YpjFfz8z5CFs0Qzc=";
          PresharedKeyFile = config.sops.secrets."juspay-wg/preSharedKey".path;
          PersistentKeepalive = 20;
          Endpoint = "ofc1.connect.juspay.net:51820";
        }
      ];
    };
  };
}
