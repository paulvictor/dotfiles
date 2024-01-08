{ lib, config, pkgs, ...}:

with lib;
let
  cfg = config.services.actual-server;
in
{
  options = {
    services.actual-server = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
      port = mkOption {
        type = types.port;
        default = 5600;
        description = lib.mdDoc ''
          Specifies on which ports the actual server listens;
        '';
      };
      hostname = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = lib.mdDoc ''
          Specifies on which hostname the actual server listens;
        '';
      };
      dataDir = mkOption {
        type = types.path;
        default = "/actual";
        description = lib.mdDoc ''
          Specifies where actual stores data
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.actual-server;
        description = lib.mdDoc ''
          Specifies which package to use
        '';
      };

      configFile = mkOption {
        type = types.path;
        description = lib.mdDoc ''
          Config file path for actual server, contailing keys "dataDir", "serverFiles", "userFiles" and "webRoot"
        '';
        default = pkgs.writeText "config.json"
          (builtins.toJSON (rec {
            dataDir = config.services.actual-server.dataDir;
            serverFiles = "${dataDir}/server-files";
            userFiles = "${dataDir}/user-files";
            webRoot = "${config.services.actual-server.package}/build/node_modules/@actual-app/web/build";
          }));
      };
    };
  };
  config = mkIf cfg.enable {
    users.users.actual = {
      isSystemUser = true;
      group = "actual";
      home = "/actual";
      createHome = true;
      homeMode = "755";
    };
    users.groups.actual = {};
    systemd.services.actual = {
      description = "Actual server";
      wantedBy = [ "multi-user.target" ];
      environment = {
        ACTUAL_HOSTNAME = cfg.hostname;
        ACTUAL_CONFIG_PATH = cfg.configFile;
      };
#       path = with pkgs; [ coreutils nodejs ];
#       serviceConfig.ExecStartPre = ''
#         ${pkgs.coreutils}/bin/mkdir -pv ${cfg.dataDir}
#         ${pkgs.coreutils}/bin/chown -R actual:actual ${cfg.dataDir}
#       '';
      serviceConfig = {
        ExecStart = pkgs.writeShellScript "actual-runner"
          ''
            cd ${cfg.package}/build # This is because the path to migrations is hard coded
            ${pkgs.nodejs}/bin/node ${cfg.package}/build/app.js
          '';
        User = "actual";
        Group = "actual";
      };
    };
  };
}
