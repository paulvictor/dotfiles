# GlobalProtect VPN: a microVM tunnel on the host (NixOS), with ssh/socks client bits and a
# waybar status widget on the home-manager side. The home-manager half needs the `ssh` feature.
{ inputs, ... }: {
  flake.overlays.gp-vpn = import ./_overlay.nix;

  flake.modules.nixos.gp-vpn = { config, ... }: {
    imports = [
      inputs.microvm.nixosModules.host
      ./_host.nix
    ];
    microvm.vms.gp-tunnel = {
      autostart = true;
      config = {
        imports = [
          inputs.microvm.nixosModules.microvm
          ./_vm.nix
        ];
        _module.args.hostAuthorizedKeysFiles =
          config.users.users.${config.me.username}.openssh.authorizedKeys.keyFiles;
      };
    };
  };

  flake.modules.homeManager.gp-vpn = { config, pkgs, lib, ... }:
    let
      socks = config.custom.gpVpn.socks;
    in
    {
      options.custom.gpVpn.socks = {
        host = lib.mkOption {
          type = lib.types.str;
          default = "gp-tunnel-host";
          description = "Host running the socks proxy behind the VPN";
        };
        port = lib.mkOption {
          type = lib.types.port;
          default = 1080;
        };
      };

      config = {
        programs.ssh.settings = {
          "gp-host" = lib.hm.dag.entryBefore [ "*" ] {
            HostName = socks.host;
            UserKnownHostsFile = "/dev/null";
            User = "root";
            StrictHostKeyChecking = "no";
            PubkeyAuthentication = "no";
            PasswordAuthentication = "yes";
          };
          "*.bitbucket.juspay.net" = lib.hm.dag.entryBefore [ "*" ] {
            User = "git";
            IdentityFile = config.custom.ssh.identityFile;
            ProxyCommand = "${pkgs.netcat-openbsd}/bin/nc -x ${socks.host}:${toString socks.port} %h %p";
          };
        };

        xdg.configFile."socks/socks.conf".text = ''
          route {
            from: 0.0.0.0/0 to: 0.0.0.0/0 via: 127.0.0.1 port = 1080
            proxyprotocol: socks_v5
          }
          resolveprotocol: tcp
        '';
        home.sessionVariables.SOCKS_CONF = "${config.xdg.configHome}/socks/socks.conf";

        # A Firefox profile that goes through the tunnel; needs the `firefox` feature
        programs.firefox.profiles.proxied = { ... }: {
          imports = [ config.custom.firefox.baseProfile ];
          id = 1;
          settings = {
            "network.proxy.type" = 1;
            "network.proxy.socks" = socks.host;
            "network.proxy.socks_port" = socks.port;
            "network.proxy.socks_version" = 5;
            "network.proxy.socks_remote_dns" = true;
          };
          userChrome = lib.mkAfter ''
            #TabsToolbar {
              background-color: #6b0000 !important;
            }
            .tab-background:is([selected], [multiselected]) {
              background-color: #3a0000 !important;
            }
          '';
        };
        xdg.desktopEntries.firefox-proxied = {
          name = "Firefox (Proxied)";
          genericName = "Web Browser";
          exec = "${config.programs.firefox.package}/bin/${config.programs.firefox.package.meta.mainProgram} -P proxied --no-remote %U";
          icon = "firefox-developer-edition";
          categories = [ "Network" "WebBrowser" ];
          terminal = false;
        };

        # Second in modules-right: after kanata-layer (order 500), before cpu and the rest (order 1500)
        programs.waybar.settings.mainBar.modules-right = lib.mkOrder 1000 [ "custom/gp-vpn" ];
        programs.waybar.settings.mainBar."custom/gp-vpn" = {
          exec = "${pkgs.gpVpn}/bin/gp-vpn status";
          on-click = "${pkgs.gpVpn}/bin/gp-vpn reconnect";
          on-click-right = "${pkgs.gpVpn}/bin/gp-vpn disconnect";
          interval = 30;
          return-type = "json";
          format = "󰦝 {}";
        };
      };
    };
}
