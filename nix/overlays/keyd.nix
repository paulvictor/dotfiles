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
  keyd = prev.keyd.overrideAttrs(f: p:
    {
      version = "v2.5.0";
      inherit src;
      postPatch = builtins.replaceStrings ["keyd.service"] ["keyd.service.in"] p.postPatch;
      postInstall = ''
        ln -sv $out/local/bin $out
        ln -sv $out/local/share $out

        rm -rf $out/etc
      '';
    });
}
