# schemesh shell
{ inputs, ... }: {
  flake.overlays.schemesh = import ./_overlay.nix inputs.schemesh;

  flake.modules.homeManager.schemesh = { lib, pkgs, ... }: {
    home.packages = [ pkgs.schemesh ];
    home.activation.createSchemeshDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run mkdir -p $VERBOSE_ARG $HOME/.cache/schemesh
      run [ -f $HOME/.cache/schemesh/history.txt ] || touch $HOME/.cache/schemesh/history.txt
      run mkdir -p $VERBOSE_ARG $HOME/.config/schemesh
    '';
  };
}
