{ pkgs }:

with pkgs;

vimUtils.buildVimPlugin {
  pname = "NeoSolarized";
  version = "1af4bf6835f0fbf156c6391dc228cae6ea967053";
  src = fetchFromGitHub {
    "owner" = "icymind";
    "repo" = "NeoSolarized";
    "rev" = "1af4bf6835f0fbf156c6391dc228cae6ea967053";
    "sha256" = "1l98yh3438anq33a094p5qrnhcm60nr28crs0v4nfah7lfdy5mc2";
  };
}
