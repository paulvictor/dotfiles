{ pkgs }:

with pkgs;

vimUtils.buildVimPlugin {
  pname = "vimwiki";
  version = "v2.5";
  src = fetchFromGitHub {
    "owner" = "vimwiki";
    "repo" = "vimwiki";
    "rev" = "v2.5";
    "sha256" = "03dic4vjl3bwd7s6jj3vrz6xicf3a3bccaqgx635hn01ha6vw1jw";
  };
}
