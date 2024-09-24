{config, lib, pkgs, ...}:
with lib;
let
  cfg = config.services.stumpwm-like;
  command = "${config.home.homeDirectory}/dotfiles/stow/stumpwm-like/init.scm";
in

{
  options.services.stumpwm-like = {
    enable = mkEnableOption "stumpwm like behaviour";
  };
  config =
    lib.mkIf
      cfg.enable
      {
        systemd.user.services.stumpwm-like = {
          Unit = {
            Description = "Make sway behave like stumpwm";
            After = [ "sway-session.target" ];
          };

          Install = {
            WantedBy = [ "default.target" ];
          };

          Service = {
            ExecStart = command;
          };
        };
      };
}
