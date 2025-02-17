{config, pkgs, lib, ...}:


{
  system.activationScripts = {
    etc = {
      enable = true;
      text = ''
          # TODO check if the files exist and skip
           ${pkgs.openssh}/bin/ssh-keygen -A
           /bin/chmod 640 /etc/ssh/ssh_host_ed25519_key
           /bin/chmod 640 /etc/ssh/ssh_host_rsa_key
        '' ;
    };
  };
}
