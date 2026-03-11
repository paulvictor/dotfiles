schemesh-repo: final: prev:

let
  schemesh = final.callPackage ("${schemesh-repo}/default.nix") { pkgs = final; };
in {
  inherit schemesh;
}
