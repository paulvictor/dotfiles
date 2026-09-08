{ lib, pkgs, ... }:

{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    systemd.targets = ["sway-session.target"];
    style = builtins.readFile ./config/waybar-style.css;
    settings = {
      mainBar = {
        output = [ "*" ];
        layer = "top";
        position = "top";
        height = 30;
        modules-left = [ "sway/workspaces" "sway/mode" ];
        modules-center = [ "sway/window" ];
        "sway/window" = {
          max-length = 45;
        };
        modules-right = [ "custom/kanata-layer" "custom/gp-vpn" "cpu" "memory" "network" "clock" "battery" ];
        "custom/kanata-layer" = {
          exec = "${pkgs.kanata-layer}/bin/kanata-layer --port 1278";
          return-type = "json";
          format = "󰌌  {} ";
        };
        "custom/gp-vpn" = {
          exec = "${pkgs.gpVpn}/bin/gp-vpn status";
          on-click = "${pkgs.gpVpn}/bin/gp-vpn reconnect";
          on-click-right = "${pkgs.gpVpn}/bin/gp-vpn disconnect";
          interval = 30;
          return-type = "json";
          format = "󰦝 {}";
        };
        clock = {
          interval = 5;
          tooltip = false;
          format = "{:%a, %b %d %R}";
        };
        "sway/mode" = {
          format = " {}";
          max-length = 20;
        };
        cpu = {
          interval = 10;
          max-length = 10;
          format = "   {usage}%";
        };
        memory = {
          format = " 💾 {used:0.1f}G";
        };
        battery = {
          bat = lib.mkDefault "BAT0";
          interval = 15;
          states = {
            good = 95;
            warning = 30;
            critical = 15;
          };
          format = "{icon} {capacity}%";
          format-charging = "⚡ {capacity}%";
          format-icons = ["" "" "" "" ""];
        };
        network = {
          format-wifi = "<span color='#589df6'>⇵</span> {bandwidthUpBits}/{bandwidthDownBits}";
          format-ethernet = "⇵ {bandwidthUpBits}/{bandwidthDownBits}";
          format-linked = "{ifname} (No IP)";
          format-disconnected = "Disconnected ⚠";
          tooltip-format-wifi = "{essid}  {signalStrength}%";
          tooltip-format-ethernet = "{ifname}: {ipaddr}/{cidr}";
        };
      };
    };
  };
}
