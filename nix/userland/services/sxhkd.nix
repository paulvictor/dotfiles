{config, lib, ...}:

let
  cfg = config.services.sxhkd;
in lib.mkIf cfg.enable
  (let
    sxhkdCommand = "${cfg.package}/bin/sxhkd ${toString cfg.extraOptions}";
   in
    {
      systemd.user.services.sxhkd = {
        Unit = {
          Description = "SXHKD";
          BindsTo = [ "graphical-session.target" ];
          After = [ "graphical-session-pre.target" ];
        };

        Install = {
          WantedBy = [ "default.target" ];
        };

        Service = {
          #       ExecStartPre = "${pkgs.coreutils}/bin/sleep 2";
          ExecStart = sxhkdCommand;
        };
      };
    })
