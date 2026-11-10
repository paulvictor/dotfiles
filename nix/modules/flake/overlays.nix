# `overlays.default` is every overlay defined under `flake.overlays.*`, composed in name order.
# Features define their own overlay next to the rest of their config.
{ lib, config, ... }: {
  flake.overlays.default =
    lib.composeManyExtensions
      (lib.attrValues (lib.removeAttrs config.flake.overlays [ "default" ]));
}
