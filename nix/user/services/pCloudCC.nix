{config, lib, ... }:
let
  cfg = config.services.pCloudCC;
in
with lib;
{
  options = {
    services.pCloudCC = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
      loginId = mkOption {
        type = types.str;
        default = null;
      };
      mountPoint = mkOption {
        type = with types; nullOr path;
        default = null;
      };
      package = mkOption {
        type = types.package;
        default = null;
      };
    };
  };
  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    systemd.user.services.pCloudCC = {
      Unit = {
        Description = "pCloudCC Service";
        After = [ "network.target" ];
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = "${cfg.package}/bin/pcloudcc -u ${cfg.loginId} -m ${cfg.mountPoint} --daemonize";
        Type = "forking";
      };
    };
  };
}
