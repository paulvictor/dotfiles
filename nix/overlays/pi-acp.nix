final: prev:

{
  pi-acp = final.buildNpmPackage rec {
    pname = "pi-acp";
    version = "0.0.34";
    src = final.fetchFromGitHub {
      owner = "svkozak";
      repo = "pi-acp";
      rev = "v${version}";
      hash = "sha256-QRwxOtTZOY+Np3PkAoy2o2PrUzEqjItM/372sCPlSMo=";
    };
    npmDepsHash = "sha256-BvLNtFfp1cMVjzWcMRSdhTqiJrTfbFoUbWkkPW9200o=";
    nativeBuildInputs = [ final.makeWrapper ];
    # pi-acp spawns `pi`; default to the nixpkgs pi-coding-agent binary
    postInstall = ''
      wrapProgram $out/bin/pi-acp \
        --set-default PI_ACP_PI_COMMAND ${final.pi-coding-agent}/bin/pi
    '';
    meta = {
      description = "ACP adapter for pi coding agent";
      homepage = "https://github.com/svkozak/pi-acp";
      license = final.lib.licenses.mit;
      mainProgram = "pi-acp";
    };
  };
}
