{config, lib, pkgs, inputs, ...}:

let
  shared = import ./shared.nix;
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

  networking.hostName = "patriot";
  microvm.shares = [
    {
      tag = "ro-store";
      source = "/nix/store";
      mountPoint = "/nix/.ro-store";
    }
    # TODO share wg keys
  ];
}
