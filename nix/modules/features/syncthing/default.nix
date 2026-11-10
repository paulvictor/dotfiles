# File sync; only enabled on hosts that have an encrypted key in secrets/<host>/syncthing.yaml
{
  flake.modules.nixos.syncthing = {
    imports = [ ./_syncthing.nix ];
  };
}
