{ config, lib, pkgs, inputs, ... }:

let
  script = pkgs.writeShellScript "power-mgmt" "
    /usr/bin/pmset -a hibernatemode 0
    /usr/bin/pmset -a sleep 0
    /usr/bin/pmset -a disablesleep 1
  ";
  programWrapper = import ./common.nix { inherit pkgs; };
in
{
  launchd.daemons = {
    nix-power-settings-change.serviceConfig = {
      Label = "daemon.nix.power-management";
      ProgramArguments = programWrapper { program = script; name = "power-mgmt"; };
      RunAtLoad = true;
    };
  };
}

