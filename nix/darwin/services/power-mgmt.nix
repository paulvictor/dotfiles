{ config, lib, pkgs, inputs, ... }:

let
  script = pkgs.writeShellScript "power-mgmt" "
    /usr/bin/pmset -a hibernatemode 0
    /usr/bin/pmset -a sleep 0
    /usr/bin/pmset -a disablesleep 1
  ";
in
{
  launchd.daemons = {
    nix-power-settings-change.serviceConfig = {
      Label = "daemon.nix.power-management";
      ProgramArguments = [
        "${script}"
      ];
      RunAtLoad = true;
    };
  };
}

