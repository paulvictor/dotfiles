final: prev:

let
  src = final.fetchFromGitHub {
    owner = "atlas-engineer";
    repo = "nyxt";
    rev = "07f2a699bdf99b3a62bab6dabcc0933470b4a244";
    sha256 = "UH0FtCd51tJUpN0iRcDykX8UFJK4LRT2AvNPZ7/mS2U=";
  };
in
{
  nyxt = prev.lispPackages.nyxt.overrideAttrs(old: {
    version = "2022-07-29";
    inherit src;
    buildInputs = old.buildInputs ++ [ final.makeWrapper ];
    postInstall = old.postInstall + "\n" + ''
      wrapProgram $out/bin/nyxt \
        --set-default WEBKIT_FORCE_SANDBOX 0
    '';

  });
}
