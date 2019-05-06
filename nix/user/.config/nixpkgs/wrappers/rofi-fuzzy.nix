self: super:

{
  rofi = super.stdenv.mkDerivation {
    name = "rofi-fuzzy";
    buildInputs = [ super.makeWrapper ];
    preferLocalBuild = true;
    buildCommand = ''
      mkdir -pv $out/bin
      makeWrapper ${super.rofi}/bin/rofi $out/bin/rofi --add-flags "-matching fuzzy"
    '';
  };
}
