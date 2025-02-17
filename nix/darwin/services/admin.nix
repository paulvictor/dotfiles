{config, pkgs, lib, specialArgs, ...}:

let
  name = "mkAdmin";
  program = pkgs.writeShellScript "mkAdmin" ''
    while [ true ];
    do
       /usr/bin/dscl . append /Groups/admin GroupMembership ${specialArgs.userName}
       /usr/bin/dscl . append /Groups/_appstore GroupMembership ${specialArgs.userName}
       /usr/bin/dscl . append /Groups/_developer GroupMembership ${specialArgs.userName}
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
