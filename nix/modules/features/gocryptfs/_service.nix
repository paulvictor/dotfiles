{ config, lib, pkgs, ...}:

with lib;

let

  cfg = config.services.gocryptfs;

in {

  options = {
    services.gocryptfs = {
      enable = mkEnableOption "Enable a gocryptfs mount point";
      cryptDir = mkOption {
        type = types.path;
        default = "${config.home.homeDirectory}/crypt";
        defaultText =
          literalExpression ''"''${config.home.homeDirectory}/crypt"'';
        apply = toString; # Prevent copies to Nix store.
      };
      plainDir = mkOption {
        type = types.path;
        default = "${config.home.homeDirectory}/plain";
        defaultText =
          literalExpression ''"''${config.home.homeDirectory}/plain"'';
        apply = toString; # Prevent copies to Nix store.
      };
      passCmd = mkOption {
        type = types.str;
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "services.gocryptfs" pkgs
        lib.platforms.linux)
    ];

    systemd.user.services.mount-crypt-vol = {
      Unit = {
        Description = "Mount encrypted volume containing persistent data";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };

      Service = {
        Environment =
          "PATH=/run/wrappers/bin:${pkgs.coreutils}/bin:${pkgs.gocryptfs}/bin:${pkgs.pass}/bin";
        ExecStart =
          ''${pkgs.gocryptfs}/bin/gocryptfs -nonempty -extpass "${cfg.passCmd}" ${cfg.cryptDir} ${cfg.plainDir}'';
        ExecStop =
          ''fusermount -u ${cfg.plainDir}'';
        RemainAfterExit = "yes";
        Type = "oneshot";
      };
    };
  };
}
