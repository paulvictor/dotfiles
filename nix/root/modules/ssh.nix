{ lib, ... }:

{
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = lib.mkDefault "yes";
      GatewayPorts = "yes";
      PasswordAuthentication = false;
      StrictModes = false;
    };
  };
}
