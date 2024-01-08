{ actual-server-repo }:

final: prev:

let
  system = final.system;
in {
  actual-server = actual-server-repo.packages.${system}.default;
}
