{ config, lib, pkgs, inputs, ... }:

{
  launchd.daemons = {
    nix-tun0.serviceConfig = {
      Label = "daemon.nix.tun-0";
      UserName = "paul";
      ProgramArguments = [
        "${pkgs.openssh}/bin/ssh"
        "-nNT"
        "-i"
        "/Users/paul/custom-ssh/temp-key"
        "-R"
        "0.0.0.0:8080:localhost:2224"
        "-l"
        "viktor"
        "143.244.136.171"
      ];
      RunAtLoad = true;
      KeepAlive = true;
    };
    nix-tun1.serviceConfig = {
      Label = "daemon.nix.tun-1";
      UserName = "paul";
      ProgramArguments = [
        "${pkgs.openssh}/bin/ssh"
        "-nNT"
        "-i"
        "/Users/paul/custom-ssh/temp-key"
        "-R"
        "127.0.0.1:5900:localhost:5900"
        "-l"
        "viktor"
        "143.244.136.171"
      ];
      RunAtLoad = true;
      KeepAlive = true;
    };
  };

}

