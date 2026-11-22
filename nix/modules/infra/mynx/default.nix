# The mynx infrastructure: a DigitalOcean droplet and cloud firewall, and the DNS record on Cloudflare.
#
#   nix run .#mynx.plan                    # what would change
#   nix run .#mynx.apply
#   nix run .#mynx.terraform -- state list # any other tofu command
#
{ self, ... }: {
  perSystem = { pkgs, ... }:
    let
      host = self.nixosConfigurations.mynx.config;
      # What the parts need from the rest of the flake
      args = {
        inherit host;
        # What deploy-rs and ssh connect to, and the name NetBird is served on
        hostname = self.deploy.nodes.mynx.hostname;
        domain = host.services.netbird.server.domain;
        zone = "paulvictor.foo";
      };
    in
    {
      terranix.terranixConfigurations.mynx = {
        workdir = ''"''${XDG_STATE_HOME:-$HOME/.local/state}/terranix/mynx"'';

        terraformWrapper = {
          # Providers come from nixpkgs, so init needs no downloads and the versions are pinned by flake.lock
          package = pkgs.opentofu.withPlugins (p: [ p.digitalocean_digitalocean p.cloudflare_cloudflare ]);
          extraRuntimeInputs = [ pkgs.pass ];
          prefixText = ''
            export DIGITALOCEAN_TOKEN="''${DIGITALOCEAN_TOKEN:-$(pass show digitalocean.com/paulvictor@gmail.com/access-token/terranix)}"
            export CLOUDFLARE_API_TOKEN="''${CLOUDFLARE_API_TOKEN:-$(pass show cloudflare/paulvictor@gmail.com/api_token/terranix)}"
          '';
        };

        modules = [
          { _module.args = args; }
          ./_providers.nix
          ./_droplet.nix
          ./_firewall.nix
          ./_dns.nix
        ];
      };
    };
}
