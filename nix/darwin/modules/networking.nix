args@{config, pkgs, lib, ...}:

{
  networking =
    {
      dns = [ "8.8.8.8" "8.8.4.4" ];
    };
}
