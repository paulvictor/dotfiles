# Keyboard tinkering: QMK/VIA firmware access, and software remapping (kanata, keyd)
{
  allowedUnfree = [ "via" "vial" ];

  # Firmware flashing and configuration: udev rules, groups, entropy
  flake.modules.nixos.keyboard-hacking = { config, pkgs, ... }: {
    programs.entropy.enable = true;
    hardware.keyboard.qmk.enable = true;
    environment.systemPackages = with pkgs; [via vial];
    services.udev.packages = with pkgs; [
      via
      vial
      qmk-udev-rules
      qmk qmk_hid
    ];
    users.users.${config.me.username}.extraGroups = [ "plugdev" config.programs.entropy.group ];
    services.udev.extraRules = ''
      # Match RMK / pid.codes Vendor ID
      KERNEL=="hidraw*", ATTRS{idVendor}=="1209", MODE="0660", GROUP="plugdev", TAG+="uaccess"

      # Match generic raw HID interfaces with Vial enabled
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{vi_enabled}=="1", MODE="0660", GROUP="plugdev", TAG+="uaccess"
    '';
  };

  # kanata with a per-host layout (_kanata-<hostname>.lisp); enabled on every host that has a layout file
  flake.modules.nixos.kanata = {lib, config, pkgs, ...}: {
    services.kanata = {
      enable = lib.pathExists ./_kanata-${config.networking.hostName}.lisp;
      keyboards.builtin = {
        config = builtins.readFile ./_kanata-${config.networking.hostName}.lisp;
        port = 1278;
        extraArgs = [ ];
        extraDefCfg = "
          process-unmapped-keys   yes
          concurrent-tap-hold     yes
          allow-hardware-repeat   yes
          log-layer-changes       no
          movemouse-inherit-accel-state yes
          movemouse-smooth-diagonals yes
        ";
      };
    };
  };

  # keyd, only active on hosts that have an entry in _keyd-device-mappings.nix
  flake.modules.nixos.keyd = args@{ pkgs, specialArgs, lib, config, ...}:

    let
      allKeyboardsConfigs = import ./_keyd-device-mappings.nix;
      thisDeviceConfig = allKeyboardsConfigs.${config.networking.hostName} or null;
    in
      {
        environment.systemPackages = [ pkgs.keyd ];
        #systemd.services.keyd.wantedBy = lib.mkForce [];
        services.keyd = {
          enable = !(isNull thisDeviceConfig);
          keyboards = thisDeviceConfig;
        };
      };
}
