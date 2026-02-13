{config, lib, pkgs, inputs, ...}:

let
  shared = import ./shared.nix;
  tunDevice = "gptun";
  csdWrapper =
    with pkgs;
    runCommandLocal "hipreport-with-hostid" {
      nativeBuildInputs = [gnused coreutils];
    } ''
        mkdir $out
        cat ${pkgs.openconnect}/libexec/openconnect/hipreport.sh | sed 's|^HOST_ID=.*|HOST_ID=''$(hostid)|' > $out/hipreport.sh
        chmod a+x $out/hipreport.sh
      '';
in
{
  imports = [ shared.vm.wg ];
  systemd.network.enable = true;
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  services.resolved = {
    enable = true;
    settings = {
      Resolve.LLMNR = "true";
      Resolve.FallbackDNS = [];
    };
  };
  users.users.root.password = "toor";
  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
  };
  system.stateVersion = config.system.nixos.release;
  boot.kernelPackages = pkgs.linuxPackages_latest;
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
  networking.hostName = "patriot";
  networking.openconnect.interfaces.${tunDevice} = {
    user= "paul.victor@identity.juspay.net";
    protocol = "gp";
    gateway = "pv-ext.connect.juspay.net";
    extraOptions = {
      csd-wrapper = "${csdWrapper}/hipreport.sh";
      disable-ipv6 = true;
    };
    passwordFile = pkgs.writeText "password" "podaaanga";
  };
  services.dante = {
    enable = true;
    config = ''
      internal: ${shared.wgAddress.vm} port = 1080
      internal: ${shared.vmLease} port = 1080
      external: ${tunDevice}
      clientmethod: none
      socksmethod: none
      client pass {
        from: 0.0.0.0/0 to: 0.0.0.0/0
	      log: error # connect disconnect
      }
      socks pass {
        from: 0.0.0.0/0 to: 0.0.0.0/0
        command: bind connect udpassociate
        log: error # connect disconnect iooperation
      }
      socks pass {
        from: 0.0.0.0/0 to: 0.0.0.0/0
        command: bindreply udpreply
        log: error # connect disconnect iooperation
      }
    '';
  };
  systemd.services.dante.bindsTo =
    [
      "sys-devices-virtual-net-${tunDevice}.device"
      "wg-quick-to-host.service"
    ];
  systemd.services.dante.requires =
    [
      "sys-devices-virtual-net-${tunDevice}.device"
      "wg-quick-to-host.service"
    ];
  microvm.shares = [
    {
      tag = "ro-store";
      source = "/nix/store";
      mountPoint = "/nix/.ro-store";
    }
    # TODO share wg keys and password for gp
  ];
}
