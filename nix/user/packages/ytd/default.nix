{ pkgs ? import <nixpkgs> {}}:
with pkgs;
buildGoPackage rec {
  pname = "yubikey-touch-detector";
  version = "1.9.0";

  goPackagePath = "github.com/maximbaz/yubikey-touch-detector";

  src = fetchFromGitHub {
    owner = "maximbaz";
    repo = "yubikey-touch-detector";
    rev = "eea82447cd7d701df1a77bd31b0a238b2d918099";
    sha256 = "0cs7qbr820ssys4dk53g4fvcyiw2vr1139773nbyyq488iv9mln0";
    fetchSubmodules = true;
  };

  goDeps = ./deps.nix;

  nativeBuildInputs = with pkgs; [ pkg-config ];
  buildInputs = with pkgs; [ libnotify ];

  buildFlags = [ "--tags" "release" ];
}

