{config, pkgs, lib, ...}:

let
  name = "mkAdmin";
  userName = "paul.victor"; # This is the username which is in all my macs, can it be figured from config?
  program = pkgs.writeShellScript "mkAdmin" ''
    while [ true ];
    do
       /usr/bin/dscl . append /Groups/admin GroupMembership ${userName}
       /usr/bin/dscl . append /Groups/_appstore GroupMembership ${userName}
       /usr/bin/dscl . append /Groups/_developer GroupMembership ${userName}
       /bin/sleep 10
    done;
  '';
  programWrapper = import ./common.nix { inherit pkgs; };
  wrappedProgram = programWrapper { inherit program name; };
in
{
  launchd.daemons = {
    mkAdmin.serviceConfig = {
      Label = "daemon.nix.mkAdmin";
      ProgramArguments = wrappedProgram;
      RunAtLoad = true;
    };
  };
}
