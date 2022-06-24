{ config, lib, pkgs, inputs, ... }:

{
  launchd.daemons = {
    nix-sshd.serviceConfig = {
      Label = "daemon.nix.sshd";
      UserName = "paul";
      ProgramArguments = [
        "${pkgs.openssh}/bin/sshd"
        "-D"
	"-f"
	"/Users/paul/.custom-ssh/sshd_config"
      ];
      RunAtLoad = true;
      KeepAlive = true;
    };
  };
}
