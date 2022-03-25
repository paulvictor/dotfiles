{pkgs, config, ...}:

let
  cryptDir = "${config.home.homeDirectory}/crypt";
  plainDir = "${config.home.homeDirectory}/plain";
in {
  systemd.user.services.mount-crypt-vol = {
    Unit = {
      Description = "Mount encrypted volume containing persistent data";
    };

    Install = {
      WantedBy = [ "default.target" ];
    };

    Service = {
      Environment =
        [ "PATH=/run/wrappers/bin:${pkgs.coreutils}/bin:${pkgs.gocryptfs}/bin:${pkgs.pass}/bin" ];
      ExecStart =
        ''${pkgs.gocryptfs}/bin/gocryptfs -badname '*.sync-conflict*' -nonempty -extpass "cat /run/secrets/crypt-mount-key" ${cryptDir} ${plainDir}'';
      ExecStop =
        ''fusermount -u ${plainDir}'';
      RemainAfterExit = "yes";
      Type = "oneshot";
    };
  };
}
