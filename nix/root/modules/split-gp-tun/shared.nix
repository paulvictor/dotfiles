let
  wgPort = 51820;
  # TODO generate these as part of activationScripts
  hostKeys = {
    priv = "MJWd2oYFwGv4K2F95ExVCyRjA64wUwbemhVORWoCc20="; # "/run/shared/wg/host/priv";
    pub = "nXoiZoHgKhbCxdXOlLCZyT/7FHnBzzQH1sa4rbLYVnY="; # "/run/shared/wg/host/pub";
  };
  vmKeys = {
    priv = "qCcWcWLvQnxhUiqDifeESknESRoUny1MrIvM5vnIOUY="; # "/run/shared/wg/vm/priv";
    pub = "RVivXjDvKZPyZXnvVq+5s+lRWqQg/zcX/+ZUnSgOygs=" ; # "/run/shared/wg/vm/pub";
  };
  ipPrefix = "172.16.55";
  bridgeIp = "${ipPrefix}.1";
  vmMac = "02:00:00:00:00:01"; # How do i make this unique?
  vmLease = "${ipPrefix}.18";
  wgAddress = {
    host = [ "172.16.100.1/32"];
    vm = [ "172.16.100.2/32"];
  };
in {
  inherit ipPrefix bridgeIp vmMac vmLease;
  host.wg = {config, lib, ...}:
    {
      # nix.settings.extra-sandbox-paths = [
      #         "/run/shared/wg"
      #       ];
      networking.wg-quick.interfaces.to-vm = {
        autostart = false;
        privateKey = hostKeys.priv;
        address = wgAddress.host;
        listenPort = wgPort;
        peers = [{
          publicKey = vmKeys.pub;
          endpoint = "${vmLease}:${toString wgPort}";
          allowedIPs = wgAddress.vm;
        }];
      };
    };
  vm.wg = {config, lib, pkgs, ...}:
    {
      system.stateVersion = config.system.nixos.release;
      boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;
      programs.htop.enable = true;
      programs.neovim = {
        enable = true;
        vimAlias = true;
        viAlias = true;
      };

      programs.tmux = {
        enable = true;
        secureSocket = false;
      };

      nix.extraOptions = ''
        keep-outputs = true
        keep-derivations = true
        experimental-features = nix-command flakes
        accept-flake-config = true
      '';
      nix.package = pkgs.nixVersions.latest;

      environment.systemPackages = with pkgs;[ jq bind ];
      networking.firewall.enable = false;

      #      nix.settings.extra-sandbox-paths = [
      #         "/run/shared/wg"
      #       ];
      networking.wg-quick.interfaces.to-host = {
        autostart = false;
        privateKey = vmKeys.priv;
        address = wgAddress.vm;
        listenPort = wgPort;
        peers = [{
          publicKey = hostKeys.pub;
          endpoint = "${bridgeIp}:${toString wgPort}";
          allowedIPs = wgAddress.host;
        }];
      };
    };
}
