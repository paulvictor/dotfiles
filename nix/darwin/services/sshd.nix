{ config, lib, pkgs, inputs, ... }:

let
  programWrapper = import ./common.nix { inherit pkgs; };
  command = ''${pkgs.openssh}/bin/sshd -D -f /Users/paul/.custom-ssh/sshd_config'';
in
{
  launchd.daemons = {
    nix-sshd.serviceConfig = {
      Label = "daemon.nix.sshd";
      UserName = "paul";
      ProgramArguments = programWrapper { program = command; name = "sshd";};
      RunAtLoad = true;
      KeepAlive = true;
    };
  };
}
