# DigitalOcean droplet (2 GB, x86_64) that hosts the netbird server.
# Lean on purpose: `minimal` plus srvos' server settings, no home-manager. Build the image with
#   nix build .#nixosConfigurations.mynx.config.system.build.digitalOceanImage
{ config, inputs, ... }: {
  hosts.mynx = {
    profiles = with config.profiles; [ minimal ];
    stateVersion = "26.11";

    # deploy-rs connects here; the DNS record is created by modules/infra/mynx.nix
    deploy.hostname = "mynx.paulvictor.foo";

    nixos.features = with config.features.nixos; [ netbird-server ];

    nixos.modules = [
      inputs.srvos.nixosModules.server

      ({ lib, modulesPath, ... }: {
        imports = [ (modulesPath + "/virtualisation/digital-ocean-image.nix") ];

        # The system comes from this flake, not from DigitalOcean user data
        virtualisation.digitalOcean.rebuildFromUserData = false;

        # Where our shared settings or srvos' defaults do not fit a public droplet
        networking.useNetworkd = false; # keep the DHCP setup the DigitalOcean image is tested with
        boot.loader.grub.configurationLimit = 5; # a droplet needs few generations (our default is 10, srvos 5)
        boot.loader.systemd-boot.configurationLimit = 5;

        # 2 GB of RAM: compressed swap in memory as a cheap safeguard
        zramSwap.enable = true;

        services.netbird.server.domain = "vpn.mynx.paulvictor.foo";
      })
    ];

    home.enable = false;
  };
}
