final: prev:

let
  src = final.fetchFromGitHub {
    owner = "rvaiya";
    repo = "keyd";
    rev = "v2.5.0";
    hash = "sha256-pylfQjTnXiSzKPRJh9Jli1hhin/MIGIkZxLKxqlReVo=";
  };
in
{
  # This still doesnt build the man pages properly, investigate
  keyd = prev.keyd.overrideAttrs(f: p:
    {
      version = "v2.5.0";
      inherit src;
      postPatch = builtins.replaceStrings ["keyd.service"] ["keyd.service.in"] p.postPatch;
      postInstall = ''
        ln -sv $out/local/bin $out
        rm -rf $out/etc
      '';
    });
}
