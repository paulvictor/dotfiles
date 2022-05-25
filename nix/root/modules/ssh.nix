{ config, lib, pkgs, specialArgs, ... } :


{
  services.openssh = {
    enable = true;
    forwardX11 = true;
    permitRootLogin = "yes";
    gatewayPorts = "yes";
    authorizedKeysCommand = with pkgs;
      let
        script =
          writeShellScript
            "getMyKey"
            "${curl}/bin/curl -L https://github.com/paulvictor.keys";
      in toString script;
    hostKeys =
      [
        {
          path = "/tomb/${config.networking.hostName}/ssh/ssh_host_rsa_key";
          type = "rsa";
          bits = 4096;
        }
      ];
  };
}
