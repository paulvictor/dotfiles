# TODO integrate https://github.com/NixOS/nixpkgs/blob/0666613873756c1dd34bfbb1c2108b0081502f64/pkgs/applications/networking/browsers/nyxt/default.nix
final: prev:
{
  nyxt = prev.nyxt.src.override {
    src = final.fetchFromGitHub {
      owner = "atlas-engineer";
      repo = "nyxt";
      rev = "3.11.3";
      hash = "sha256-KkVn2sTvEYD9OYPezlckMdhMvQ2LKETwmsn+P1Ti424=";
    };
  }
}
