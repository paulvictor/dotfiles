{config, lib, inputs, ...}:

{
  microvm.vms = {
    gp-tunnel-host = {
      config = {
        imports = [ # Include the microvm module
          inputs.microvm.nixosModules.microvm
        ];
        systemd.network.enable = true;

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
