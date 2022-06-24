args@{config, pkgs, lib, ...}:

{
  networking =
    {
      hostName = args.hostName;
      dns = [ "8.8.8.8" "8.8.4.4" ];
}
