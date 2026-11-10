# My personal extras, on top of the minimal, graphical and developer profiles.
# Hosts add the specialised bits (gp-vpn, keyboard-hacking, arduino, ...) on top.
{ config, ... }: {
  profiles.workstation = {
    nixos = with config.features.nixos; [ secrets tailscale syncthing kanata keyd ];
    home = with config.features.home; [ electron-apps warpd ];
  };
}
