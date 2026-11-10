# Provides the `flake.modules.<class>.<name>` option every feature writes to
{ inputs, ... }: {
  imports = [ inputs.flake-parts.flakeModules.modules ];
  systems = inputs.nixpkgs.lib.systems.flakeExposed;
}
