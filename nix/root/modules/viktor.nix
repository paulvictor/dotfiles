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
    extraGroups = [ "networkmanager" "audio" "wheel" "tty" "lp" "fuse" "docker" "adbusers" "netdev" "lxd" "disk" "video" "keys" "libvirtd" "qemu-libvirtd" "pipewire" ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keyFiles =
      let
        ghKey =
            pkgs.fetchurl {
              url = https://github.com/paulvictor.keys;
              sha256 = "09xls7hcnas700p5jd1g1p91s9f12bar9hlygvbzjy3yyjn0zg9f";
            };
      in
        [ ghKey ];
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
