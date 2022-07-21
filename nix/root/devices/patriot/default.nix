{ config, lib, pkgs, ... }:

let
  resize = pkgs.writeScriptBin "resize" ''
    if [ -e /dev/tty ]; then
      old=$(stty -g)
      stty raw -echo min 0 time 5
      printf '\033[18t' > /dev/tty
      IFS=';t' read -r _ rows cols _ < /dev/tty
      stty "$old"
      stty cols "$cols" rows "$rows"
    fi
  ''; # https://unix.stackexchange.com/questions/16578/resizable-serial-console-window
in
{
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "22.05"; # Did you read the comment?

  # Set in devices/default.nix
#   networking.hostName = "sarge";
  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId =  "c0e166df";

  imports = [
#     <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
#     <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  services.qemuGuest.enable = true;

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
    autoResize = true;
  };

  boot = {
    growPartition = true;
    kernelParams = [ "console=ttyS0" ];
    loader.grub.device = "/dev/vda";
    loader.timeout = 0;
  };

  virtualisation = {
    diskSize = 8000; # MB
    memorySize = 2048; # MB
    writableStoreUseTmpfs = false;
  };

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";

  environment.systemPackages = with pkgs;
    [
      vim resize
    ];

  # we could alternatively hook root or a custom user
  # to some ssh key pair
  users.extraUsers.root.password = ""; # oops
  users.mutableUsers = false;
  virtualisation.graphics = false;
  virtualisation.qemu.options = [ "-serial mon:stdio" ];

  environment.loginShellInit = "${resize}/bin/resize";
}
