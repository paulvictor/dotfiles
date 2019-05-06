{ pkgs }:

with pkgs;
let deps = [
  (i3lock-color.overrideAttrs(attrs: { postInstall = ""; }))
  imagemagick
  xorg.xdpyinfo
  xorg.xrandr
  feh
  bc
]; in
stdenv.mkDerivation {
  src = fetchFromGitHub {
    "owner" = "pavanjadhaw";
    "repo" = "betterlockscreen";
    "rev" = "643ea70aa77686ebe84889943a258b890dffe2fb";
    "sha256" = "1i6qc2aqfpjbxk5r3dzlbm1mjy9nd0j5k6jykbnzx8kyg8j030vr";
  };
  name = "betterlockscreen";
  buildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -pv $out/bin
    makeWrapper $src/betterlockscreen $out/bin/betterlockscreen --prefix PATH : ${lib.makeBinPath deps}
  '';
}
