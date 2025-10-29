{ config, lib, pkgs, ... } :
let
  mySuperStupidPasswdHashed = "$6$SCMbhhof$227ZIsJWgaZmuZX3gwWUTv4E5VrPaVKmZ/97cbU6yclJdn7To3F0ngRAcvmYX5mPOunW8bU6v16vqvxkqjivK.";
in

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;
  users.users.root = {
    shell = pkgs.zsh;
    hashedPassword = mySuperStupidPasswdHashed;
  };
  users.users.viktor = {
    createHome = true;
    isNormalUser = true;
    hashedPassword = mySuperStupidPasswdHashed;
    uid = 1000;
    extraGroups = [ "networkmanager" "audio" "wheel" "tty" "lp" "fuse" "docker" "adbusers" "netdev" "lxd" "disk" "video" "keys" "libvirtd" "qemu-libvirtd" "pipewire" "ydotool" "ykusers" "kvm" ];
    shell = pkgs.zsh;
  };

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;
  security.sudo.extraConfig = ''
    viktor ALL=(ALL) NOPASSWD: ALL
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
