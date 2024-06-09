final: prev:

let
  src = final.fetchFromGitHub {
    owner = "rvaiya";
    repo = "keyd";
    rev = "ce1bab682baf98396b957b4c7950c89413b4c96f";
    hash = "sha256-0zlz4qlfj86LdzXPQFo8j8AUdLn2lScNXRGO7x4aHVg=";
  };
in
{
  keyd = prev.keyd.overrideAttrs(f: p: {version = "ce1bab"; inherit src; });
}
