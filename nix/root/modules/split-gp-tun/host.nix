{config, lib, pkgs, ...}:

let
  bridge = "gp-br";
  shared = import ./shared.nix;
  inherit (shared) ipPrefix bridgeIp vmMac vmLease;
  dhcpServerStaticLeases = [
    {
      MACAddress = vmMac;
      Address = vmLease;
    }
  ];
in {
  imports = [ shared.host.wg ../wireproxy.nix ];
  networking.networkmanager = {
    unmanaged = ["interface-name:${bridge}"];
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
      if config.wireless.enable or config.networking.wireless.iwd.enable
      then "w+"
      else "e+";
  };
  networking.extraHosts = ''
    ${vmLease} gp-tunnel-host
  '';
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
        addresses = [ {Address = "${bridgeIp}/24";} ];
        dhcpServerConfig = {
          PersistLeases = true;
          DNS = bridgeIp;
          EmitDNS = true;
          EmitRouter = true;
          Router = bridgeIp;
        };
        inherit dhcpServerStaticLeases;
      };
      microvm-eth0 = {
        matchConfig.Name = "vm-*";
        networkConfig.Bridge = bridge;
      };
    };
  };
  microvm.vms.gp-tunnel.config = {
    microvm.interfaces = [{
      type = "tap";
      id = "vm-gp-tunnel";
      mac = vmMac;
    }];
  };
  virtualisation.libvirtd.enable = true;
}
