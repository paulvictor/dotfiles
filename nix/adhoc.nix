{pkgs ? import <nixpkgs> {}}:

with pkgs;
mkShell {
  buildInputs = [ sqlite gocryptfs ];
}
