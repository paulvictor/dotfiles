{ config, lib, pkgs, inputs, ... }:

{
  launchd.user.agents = {
    nix-gpg-agent.serviceConfig = {
      Label = "daemon.nix.gpg-agent";
      ProgramArguments = [
        "${pkgs.gnupg}/bin/gpgconf"
        "--launch"
	"gpg-agent"
      ];
      RunAtLoad = true;
    };
  };
}

