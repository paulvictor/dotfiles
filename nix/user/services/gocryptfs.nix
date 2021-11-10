{pkgs, config, ...}:

let
  cryptDir = "${config.home.homeDirectory}/crypt";
  plainDir = "${config.home.homeDirectory}/plain-backup";
in {
  systemd.user.services.mount-crypt-vol = {
    Unit = {
      Description = "Mount encrypted volume containing persistent data";
      After = [ "yubikey-touch-detector.target" ];
    };

    Service = {
      Environment =
        "PATH=/run/wrappers/bin:${pkgs.coreutils}/bin:${pkgs.gocryptfs}/bin:${pkgs.pass}/bin";
      ExecStart =
        ''${pkgs.gocryptfs}/bin/gocryptfs -extpass "${pkgs.pass}/bin/pass persistent-backup" ${cryptDir} ${plainDir}'';
      ExecStop =
        ''fusermount -u ${plainDir}'';
      RemainAfterExit = "yes";
      Type = "oneshot";
    };
  };
}
