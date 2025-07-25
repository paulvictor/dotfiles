{config, pkgs, ...}:

{
  services.tailscale = {
    enable = true;
    overrideLocalDns = true;
  };
}
