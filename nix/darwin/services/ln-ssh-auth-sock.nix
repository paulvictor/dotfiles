{ config, lib, pkgs, inputs, ... }:

{
  launchd.user.agents = {
    link-ssh-auth-sock.serviceConfig = {
      Label = "daemon.nix.link-ssh-auth-sock";
      ProgramArguments = [
        "/bin/sh"
        "-c"
	"/bin/ln -sf $HOME/.gnupg/S.gpg-agent.ssh $SSH_AUTH_SOCK"
      ];
      RunAtLoad = true;
    };
  };
}


