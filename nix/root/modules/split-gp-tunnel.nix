{config, ...}:

let
  bridge = "gp-br";
  ipPrefix = "172.16.55";
in {
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };
  networking.nat.enable = true;
  networking.nat.internalInterfaces = [ bridge ];
  networking.nat.externalInterface = "w+"; # or enp0s25
  systemd.network = {
    enable = true;
    network =  {
      networks = {
        gp-br = {
          matchConfig.Name = bridge;
          networkConfig = {
            DHCPServer = true;
            LLMNR = true;
          };
          addresses = [ {Address = "${ipPrefix}.1/24";} ];
          dhcpServerConfig = {
            PersistLeases = true;
            DNS = "${ipPrefix}.1";
            EmitDNS = true;
            EmitRouter = true;
            Router = "${ipPrefix}.1";
          };
        };
      };
    };
  };





}
