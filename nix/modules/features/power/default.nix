# Power management for machines that run on battery
{
  flake.modules.nixos.power = { pkgs, ... }: {
    services.upower.enable = true;

    services.tlp.enable = pkgs.stdenv.hostPlatform.isx86_64;

    # This will save you money and possibly your life!
    services.thermald.enable = pkgs.stdenv.hostPlatform.isx86_64;
  };
}
