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
#           BindsTo = [ "graphical-session.target" ];
          After = [ "sway-session.target" ];
        };

        Install = {
          WantedBy = [ "default.target" ];
        };

        Service = {
          ExecStart = sxhkdCommand;
        };
      };
    })
