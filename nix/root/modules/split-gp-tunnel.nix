{config, lib, pkgs, ...}:

let
  bridge = "gp-br";
  ipPrefix = "172.16.55";
  guestMac = "02:00:00:00:00:01";
  guestIpLease = "172.16.55.10";
in {
  imports = [ ./gp-tunnel-host.nix ];
  networking.networkmanager = {
    unmanaged = [
    "interface-name:${bridge}"
  ];
    enable = lib.mkForce false;
  };
  networking.useNetworkd = true;
  services.resolved.enable = true;
  environment.systemPackages = with pkgs;[
    impala iwmenu # for interacting with iwd
  ];
  networking.wireless.iwd = {
    enable = true;
    settings = {
      General = {
        EnableNetworkConfiguration = true;
      };
      Network = {
        EnableIPv6 = false;
      };
      Settings = {
        AutoConnect = true;
      };
    };
  };
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };
  networking.nat = {
    enable = true;
    internalInterfaces = [ bridge ];
    externalInterface =
      if config.wireless.enable or config. networking.wireless.iwd.enable
      then "w+"
      else "e+";
  };
  systemd.network = {
    enable = true;
    netdevs = {
      gp-br = {
        netdevConfig = {
          Kind = "bridge";
          Name = bridge;
        };
      };
    };
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
        dhcpServerStaticLeases = [
          { MACAddress = guestMac;
            Address = guestIpLease;
          }
        ];
      };
    };
  };
  microvm.vms.gp-tunnel-host.config = {
    microvm.interfaces = [{
      type = "tap";
      id = "vm-gp-tunnel";
      mac = guestMac;
    }];
  };
  virtualisation.libvirtd.enable = true;
}
