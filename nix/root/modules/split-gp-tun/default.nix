{config, lib, inputs, ...}:

{
  imports = [ ./host.nix ];
  microvm.vms.gp-tunnel = {
    autostart = true;
    config = {
      imports = [
        inputs.microvm.nixosModules.microvm
        ./vm.nix
      ];
    };
  };
}
