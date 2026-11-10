# Everyday machine basics: tools, containers, sandboxing. Also the NixOS modules from inputs that only add
# options (and do nothing until enabled), so any host can turn them on: keyfreq, entropy.
{ inputs, ... }: {
  flake.modules.nixos.basics = { config, lib, pkgs, ... }: {
    imports = [
      inputs.keyfreq.nixosModules.default
      inputs.entropy.nixosModules.default
    ];

    hardware.enableRedistributableFirmware = lib.mkDefault true;

    # keep timezone updated to local time using geoclue
    time.timeZone = null;
    services.localtimed.enable = true;
    services.geoclue2.enable = true;

    environment.systemPackages = with pkgs; [
      git
      htop
      man-pages
      man-pages-posix
      manix
      psmisc
      wget
      binutils
      bluez
      exfat
      file
      gnupg
      home-manager
      libnotify
      nix-prefetch-github
      patchelf
      pciutils
      virt-manager
      wirelesstools
    ];

    programs.neovim = {
      enable = true;
      vimAlias = true;
      viAlias = true;
    };

    virtualisation.docker = {
      enable = lib.mkDefault false;
      logDriver = "journald";
    };
    users.groups.docker = { };

    # For SSD's
    services.fstrim.enable = pkgs.stdenv.hostPlatform.isx86_64;

    programs.firejail.enable = true;

    security.wrappers = {
      cryptsetup = {
        owner = "root";
        group = "root";
        source = "${pkgs.cryptsetup}/bin/cryptsetup";
        capabilities = "cap_sys_admin+ep";
      };
    };
  };
}
