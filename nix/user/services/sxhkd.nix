{pkgs, config, lib, ...}:

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
      ExecStart = "${pkgs.sxhkd}/bin/sxhkd";
    };
  };
}
