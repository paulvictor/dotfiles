{ config, lib, pkgs, inputs, ... }:

let
  programWrapper = import ./common.nix { inherit pkgs; };
  command =
    ''${pkgs.openssh}/bin/sshd -D -f ${(toString ./.) + "/sshd_config"}'';
in
{
  imports = [
    ./gensshkeys.nix
  ];
  launchd.daemons = {
    nix-sshd.serviceConfig = {
      Label = "daemon.nix.remote";
      UserName = "paul.victor";
      ProgramArguments = programWrapper { program = command; name = "remote";};
      RunAtLoad = true;
      KeepAlive = true;
    };
  };
}
