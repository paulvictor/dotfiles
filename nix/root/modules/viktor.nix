{ config, lib, pkgs, ... } :

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;
  users.users.root = {
    shell = pkgs.zsh;
    hashedPassword = "$6$SCMbhhof$227ZIsJWgaZmuZX3gwWUTv4E5VrPaVKmZ/97cbU6yclJdn7To3F0ngRAcvmYX5mPOunW8bU6v16vqvxkqjivK.";
  };
  users.users.viktor = {
    createHome = true;
    isNormalUser = true;
    hashedPassword = "$6$SCMbhhof$227ZIsJWgaZmuZX3gwWUTv4E5VrPaVKmZ/97cbU6yclJdn7To3F0ngRAcvmYX5mPOunW8bU6v16vqvxkqjivK.";
    uid = 1000;
    extraGroups = [ "networkmanager" "audio" "wheel" "tty" "lp" "fuse" "docker" "adbusers" "netdev" "lxd" "disk" "video" "keys" "libvirtd" "qemu-libvirtd" ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys =
      [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQClUN5JbC8uK+V9aJ+JaxFNqIcBwG1WC3a2u98PfdE9lKQFNb3LoTZBfMO2qTI5Z7z8gN/A/scghnfagE85Yqt/QWKj5EoR8nnUkObw+q+HZPyPeJgUBB5yOhD6xo9vupl2sre0UiVkZNcbrgAqxymY0mPcZ4UsZgy6qEUbJSAcR6Jv5D9zHgWBQaVysxDQoN1tkacJQxdkm4rPeSwMKoTBWgCPiY4bh+jaVHTCNRRd30m9TftXkSrJGPUEBTTmPBgZDEb9MuU13c0V5PhRMnBQjAeyZMCaSvcvWLGeGQfoWT9TyUb3gBKNWKo83ds3dmUnN+QdhY5N+Z6dW4VQch+kCQjfy65fbUGc/bCsBuafTNQBx0H3fNiNWV+wkl9rDzbV99/moXSvF3M/UM/dB6Ohw9AuC8jGYDgFUZVcZf5KLnjOJQ6LV/NqxvjdEWpnIYbeB2n/DU15earOwxgLJd8jW213ebe5Weu+UX6eknHdwidhv6RnY0sWxBW4OUFEa0MmzY/lcI58qXTWkcd+fojTJRRwq6tLc50FcMRH50jsFFheIOhyRQvCUC/elU2yeK+pm7AYUlqq5oaYso9H3dqsmO0Q9e2pM+dHWeD342zC+mITMYI6Zj7s6ihjCjlz8qcQxfWaKd4Oc3yPV5XlLmdziEbr9XicB65uXwkRaWKiAQ=="
      ];
  };

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;
  security.sudo.extraConfig = ''
    viktor ALL=(ALL) NOPASSWD: ALL
    #viktor ALL=(ALL) NOPASSWD: ${pkgs.tomb}/bin/tomb*, ${pkgs.systemd}/bin/systemctl*
  '';

  nix.settings.trusted-users = [ "@wheel" "viktor" "root" ];

  environment.etc."fuse.conf" = {
    text = ''
      user_allow_other
    '';
  };

  system.build.mkViktorHomeCryptPath = pkgs.runCommandLocal "mkViktorHomeCryptPath" {} ''
    mkdir -pv $out/persist/home/viktor/
    chown -R 1000 $out/persist/home/viktor
  '';
}
