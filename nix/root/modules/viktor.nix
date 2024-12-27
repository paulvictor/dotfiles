{ config, lib, pkgs, ... } :
let
  mySuperStupidPasswdHashed = "$6$SCMbhhof$227ZIsJWgaZmuZX3gwWUTv4E5VrPaVKmZ/97cbU6yclJdn7To3F0ngRAcvmYX5mPOunW8bU6v16vqvxkqjivK.";
  ghKey =
    pkgs.fetchurl {
      url = https://github.com/paulvictor.keys;
      sha256 = "09xls7hcnas700p5jd1g1p91s9f12bar9hlygvbzjy3yyjn0zg9f";
    };
  tempKey = pkgs.fetchurl {
    url = https://gist.github.com/paulvictor/97d08c0cbdc8a96a42e2e75f19e9370e/raw/ba335efc3203d0b96399affdaee372b8e4b1b630/pub-key;
    sha256 = "02sf70y9jm9kbikw3gm45vvcriiqzii0vkdbs6pcwla25xr712ph";
  };

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
    extraGroups = [ "networkmanager" "audio" "wheel" "tty" "lp" "fuse" "docker" "adbusers" "netdev" "lxd" "disk" "video" "keys" "libvirtd" "qemu-libvirtd" "pipewire" "ydotool" "ykusers" ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keyFiles =
        [ ];
  };


  users.users.ghost = {
    isNormalUser = true;
    hashedPassword = mySuperStupidPasswdHashed;
    openssh.authorizedKeys.keyFiles = [ tempKey ];
  };

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;
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
