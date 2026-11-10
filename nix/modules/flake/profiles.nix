# `profiles.<name>` is a named, flat group of features per class, for hosts to pick from (the profiles
# themselves are in modules/profiles). Profiles do not include other profiles, and carry no settings;
# settings belong in a feature. Features are referred to through `config.features`, see features.nix.
{ lib, ... }: {
  options.profiles = lib.mkOption {
    type = lib.types.attrsOf (import ../_profile-type.nix { inherit lib; });
    default = { };
    description = "Named groups of features";
  };
}
