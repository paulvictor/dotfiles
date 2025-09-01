{ config, lib, pkgs, ...}:
with lib;
let
  batteryWarn = pkgs.callPackage ../../scripts/batteryWarn.nix {};
  cfg = config.services.batteryAlert;
in {
  options = {
    services.batteryAlert = {
      enable = mkEnableOption "Enable battery low warning";
      default = false;
    };
  };

  config = mkIf cfg.enable {
    systemd.user.timers.batteryAlert = {
      Unit.Description = "Check if battery level is too low and warn";
      Unit.Requires = "batteryAlert.service";
      Timer.OnCalendar="*-*-* *:*:00";
      Install.WantedBy = [ "timers.target" ];
    };
    systemd.user.services.batteryAlert = {
      Install.WantedBy = [ "default.target" ];
      Unit.Description = "Notify if battery is too low";
      Service = {
        ExecStart = "${batteryWarn}";
        Type = "simple";
      };
    };
  };
}
