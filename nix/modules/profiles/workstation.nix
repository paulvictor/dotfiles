# My personal and work extras, on top of the minimal, graphical and developer profiles.
# Hosts add the specialised bits (gp-vpn, keyboard-hacking, arduino, ...) on top.
{
  profiles.workstation = {
    nixos = [ "secrets" "tailscale" "syncthing" "kanata" "keyd" ];
    home = [ "electron-apps" "warpd" ];
  };
}
