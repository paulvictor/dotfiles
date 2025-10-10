final: prev:

with final;
let
  src = final.fetchFromGitHub {
    owner = "cosmos72";
    repo = "schemesh";
    rev = "d74e17a8cebbf3e55d6d16775e31b49478f9b4cc";
    hash = "sha256-j1LLggESfzIWfgiym7T1SU4hdIYqgN1luBBVztRcIFM=";
  };
  package = callPackage ("${src}/default.nix") { pkgs = final; };
in {
  schemesh = package;
}
