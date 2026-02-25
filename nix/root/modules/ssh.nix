{ lib, ... }:

{
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin =  "prohibit-password";
      GatewayPorts = "yes";
      PasswordAuthentication = false;
      PubkeyAuthentication = true;
      KbdInteractiveAuthentication = false;
      StrictModes = false;
      UsePAM = false;
      AllowUsers = [ "viktor" ];
    };
  };
  programs.mosh.enable = true;
}
