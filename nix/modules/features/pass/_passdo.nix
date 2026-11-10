final: prev:

let
  writeGuileScheme =
    { f ? "main", extraArgs ? [ "--no-auto-compile" ] }:
    let scriptArgs =
          final.lib.concatStringsSep " "
            ([ "-e" f ] ++ extraArgs  ++ [ "-s" ]);
    in
      final.writers.makeScriptWriter {
        interpreter =
          ''
            ${final.guile}/bin/guile \
            ${scriptArgs}
            !#
          '';
      };
in {
  passdo =
    writeGuileScheme
      {f = "main";}
      "/bin/passdo"
      (builtins.readFile ./passdo.scm);
}
