# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{


  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "i915" "intel_agp" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "acpi_call" ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/aa1a338a-ab76-4831-b2bc-39419646f608";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."root".device = "/dev/disk/by-uuid/d49b1a24-5ecd-46ce-ba27-f6ae73644f8c";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/12CE-A600";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;


  # acpi_call only for laptops
  boot.kernelParams = pkgs.lib.mkDefault [ "acpi_rev_override" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  boot.extraModprobeConfig = ''
    # From https://unix.stackexchange.com/questions/572962/system-setting-to-stop-wifi-from-dropping-connection and https://bugzilla.kernel.org/show_bug.cgi?id=203709
    options mac80211 probe_wait_ms=2000 beacon_loss_count=30 max_nullfunc_tries=20 max_probe_tries=50
    options iwlwifi bt_coex_active=0
  '';
}
