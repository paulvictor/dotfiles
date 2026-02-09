{config, lib, inputs, ...}:

{
  microvm.autostart = ["gp-tunnel-host"];
  microvm.vms = {
    gp-tunnel-host = {
      config = {
        imports = [
          inputs.microvm.nixosModules.microvm
        ];
        systemd.network.enable = true;
        networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
        services.resolved = {
          enable = true;
          settings = {
            Resolve.LLMNR = "true";
            Resolve.FallbackDNS = [];
          };
        };
        users.users.root.password = "toor";
        services.openssh = {
          enable = true;
          settings.PermitRootLogin = "yes";
        };
        networking.hostName = "patriot";
        microvm.shares = [ {
          tag = "ro-store";
          source = "/nix/store";
          mountPoint = "/nix/.ro-store";
        } ];
      };
    };
  };
}
