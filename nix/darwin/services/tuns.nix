{ config, lib, pkgs, inputs, ... }:

let
  programWrapper = import ./common.nix { inherit pkgs; };
  tun-0 = "0.0.0.0:8080:localhost:2224";
  tun-1 = "127.0.0.1:5900:localhost:5900";
  command = tun: ''${pkgs.openssh}/bin/ssh -NT -i /Users/paul/custom-ssh/temp-key -R ${tun} -l viktor paulvictor.xyz'';
in
{
  launchd.daemons = {
    nix-tun0.serviceConfig = {
      Label = "daemon.nix.tun-0";
      UserName = "paul";
      ProgramArguments = programWrapper { program = command tun-0; name = "tun-0"; };
      RunAtLoad = true;
      KeepAlive = true;
    };
    nix-tun1.serviceConfig = {
      Label = "daemon.nix.tun-1";
      UserName = "paul";
      ProgramArguments = programWrapper { program = command tun-1; name = "tun-1"; };
      RunAtLoad = true;
      KeepAlive = true;
    };
  };
}

