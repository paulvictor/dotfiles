# NetBird self-hosted server from nixpkgs' modules: management, signal, dashboard and relay (with STUN) as
# separate services behind nginx with a Let's Encrypt certificate. The login is the identity provider embedded in
# the management server, so no external OIDC provider is needed; the first admin is created in the browser at
# https://<domain>/setup. The host sets `services.netbird.server.domain`.
#
# The relay secret and the store encryption key are generated on first start into /var/lib/netbird-secrets.
# Back up the encryption key: without it the database cannot be read.
{
  flake.modules.nixos.netbird-server = { config, lib, pkgs, ... }:
    let
      domain = config.services.netbird.server.domain;
      secrets = "/var/lib/netbird-secrets";
      relayPort = 33080;

      websocket = {
        proxyPass = "http://127.0.0.1:${toString relayPort}";
        proxyWebsockets = true;
        extraConfig = "proxy_read_timeout 1d;";
      };
    in
    {
      services.netbird.server = {
        enable = true;
        enableNginx = true;

        management = {
          turnDomain = domain;
          # Where the embedded identity provider lives (served by the management server itself)
          oidcConfigEndpoint = "https://${domain}/oauth2/.well-known/openid-configuration";
          settings = {
            DataStoreEncryptionKey._secret = "${secrets}/store-key";
            # Relay and STUN replace coturn; no TURN servers, so its secret is unused
            TURNConfig = {
              Turns = [ ];
              Secret = null; # only used with time-based TURN credentials
            };
            Relay = {
              Addresses = [ "rels://${domain}:443" ];
              CredentialsTTL = "24h";
              Secret._secret = "${secrets}/relay-secret";
            };
            EmbeddedIdP = {
              Enabled = true;
              Issuer = "https://${domain}/oauth2";
              SignKeyRefreshEnabled = true;
              DashboardRedirectURIs = [
                "https://${domain}/nb-auth"
                "https://${domain}/nb-silent-auth"
              ];
              CLIRedirectURIs = [ "http://localhost:53000/" ];
            };
          };
        };

        # The dashboard logs in against the embedded provider
        dashboard.settings = {
          AUTH_AUTHORITY = "https://${domain}/oauth2";
          AUTH_CLIENT_ID = "netbird-dashboard";
          AUTH_AUDIENCE = "netbird-dashboard";
          AUTH_REDIRECT_URI = "/nb-auth";
          AUTH_SILENT_REDIRECT_URI = "/nb-silent-auth";
          AUTH_SUPPORTED_SCOPES = "openid profile email groups";
        };
      };

      services.netbird.relay = {
        enable = true;
        authSecretFile = "${secrets}/relay-secret";
        openFirewall = true; # STUN, UDP 3478
        settings = {
          listen-address = "127.0.0.1:${toString relayPort}";
          exposed-address = "rels://${domain}:443";
          enable-stun = true;
        };
      };

      # Generated once, kept in the state directory
      systemd.services.netbird-secrets = {
        description = "Generate the NetBird server secrets";
        wantedBy = [ "multi-user.target" ];
        before = [ "netbird-management.service" "netbird-relay.service" ];
        path = [ pkgs.openssl ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          UMask = "0077";
        };
        script = ''
          mkdir -p ${secrets}
          for name in store-key relay-secret; do
            [ -s ${secrets}/$name ] || openssl rand -base64 32 | tr -d '\n' > ${secrets}/$name
          done
        '';
      };
      systemd.services.netbird-management = {
        requires = [ "netbird-secrets.service" ];
        after = [ "netbird-secrets.service" ];
        # Without this the first start downloads GeoLite and geonames databases (a minute or more, and memory)
        # before it answers; geolocation is not needed for a personal network.
        environment.NB_DISABLE_GEOLOCATION = "true";
      };
      systemd.services.netbird-relay = {
        requires = [ "netbird-secrets.service" ];
        after = [ "netbird-secrets.service" ];
      };

      services.nginx = {
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
        virtualHosts.${domain} = {
          enableACME = true;
          forceSSL = true;
          extraConfig = ''
            client_header_timeout 1d;
            client_body_timeout 1d;
          '';
          locations = {
            "/relay" = websocket;
            "/oauth2/".proxyPass = "http://127.0.0.1:${toString config.services.netbird.server.management.port}";
          };
        };
      };

      security.acme = {
        acceptTerms = true;
        defaults.email = config.me.email;
      };

      # 80 for the certificate challenge and the redirect, 443 for everything; STUN is opened by the relay
      networking.firewall.allowedTCPPorts = [ 80 443 ];
    };
}
