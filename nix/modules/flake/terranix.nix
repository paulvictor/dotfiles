# terranix: Terraform (OpenTofu) configuration written in Nix. Each infrastructure lives in modules/infra and
# is a perSystem `terranix.terranixConfigurations.<name>`: `nix run .#<name>.plan`, `.apply`, `.destroy`.
{ inputs, ... }: {
  imports = [ inputs.terranix.flakeModule ];
}
