{ config, lib, ... }:

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
      AllowUsers = [ config.me.username ];
    };
  };
  programs.mosh.enable = true;
}
