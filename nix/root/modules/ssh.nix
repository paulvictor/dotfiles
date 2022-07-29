{ config, lib, pkgs, ... } :


{
  services.openssh = {
    enable = true;
    forwardX11 = true;
    permitRootLogin = lib.mkDefault "yes";
    gatewayPorts = "yes";
    authorizedKeysCommand =
      with pkgs;
      let
        script =
          writeShellScript
            "getMyKey"
            "${curl}/bin/curl -L https://github.com/paulvictor.keys";
      in lib.mkDefault (toString script); # Google cloud images may want to override this
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
