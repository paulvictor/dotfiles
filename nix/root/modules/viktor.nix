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
    extraGroups = [ "networkmanager" "audio" "wheel" "tty" "lp" "fuse" "docker" "adbusers" "netdev" "lxd" "disk" "video" "keys" ];
    shell = pkgs.zsh;
  };

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;
  security.sudo.extraConfig = ''
    viktor ALL=(ALL) NOPASSWD: ALL
    #viktor ALL=(ALL) NOPASSWD: ${pkgs.tomb}/bin/tomb*, ${pkgs.systemd}/bin/systemctl*
  '';

  nix.trustedUsers = [ "@wheel" "viktor" "root" ];

  environment.etc."fuse.conf" = {
    text = ''
      user_allow_other
    '';
  };
}
