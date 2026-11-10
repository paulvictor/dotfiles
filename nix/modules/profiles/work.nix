# Everything for working at Juspay: the VPN (tunnel host on NixOS, client on home-manager), mail, chat,
# and the work bits that live inside generic tools. Only for the machines that run the tunnel.
{ config, ... }: {
  profiles.work = {
    nixos = with config.features.nixos; [ gp-vpn ];
    home = with config.features.home; [ gp-vpn work-git work-tridactyl work-email work-messaging ];
  };
}
