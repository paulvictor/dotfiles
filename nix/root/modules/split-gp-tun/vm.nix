{config, lib, inputs, ...}:

let
  shared = import ./shared.nix;
in
{
  imports = [ shared.vm.wg ];
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
  microvm.shares = [
    {
      tag = "ro-store";
      source = "/nix/store";
      mountPoint = "/nix/.ro-store";
    }
    # TODO share wg keys
  ];
}
