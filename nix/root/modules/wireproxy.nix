{config, pkgs, lib, ...}:
with lib;
let
  cfg = config.services.wireproxy;
in
{
  options = {
    services.wireproxy = {
      enable = mkEnableOption "Wireproxy service";

      package = mkPackageOption pkgs "wireproxy" {};

      configFile = mkOption {
        description = ''
          Config file for the wireproxy service
        '';
        type = types.path;
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.services.wireproxy = {
      description = "Wireproxy service";
      requires = [ "network-online.target" ];
      after = [
        "network.target"
        "network-online.target"
      ];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/wireproxy --config=${cfg.configFile}";
        StandardInput =  "file:/dev/null";
      };
    };
  };
}
