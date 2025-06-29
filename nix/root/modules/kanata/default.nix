{lib, config, pkgs, ...}:

{
  services.kanata = {
    enable = true;
    keyboards.bones = {
      configFile = builtins.readFile ./minimal.cfg;
    };
  };
  # systemd.services.bones.    wantedBy = [ "multi-user.target" ];

#   systemd.services.
}
