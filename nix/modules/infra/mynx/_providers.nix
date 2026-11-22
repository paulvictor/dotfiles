# Credentials come from the environment, see the wrapper in default.nix
{
  terraform.required_providers = {
    digitalocean.source = "digitalocean/digitalocean";
    cloudflare.source = "cloudflare/cloudflare";
  };
  provider.digitalocean = { };
  provider.cloudflare = { };
}
