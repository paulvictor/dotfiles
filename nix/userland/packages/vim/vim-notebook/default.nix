{ pkgs }:

with pkgs;

vimUtils.buildVimPlugin {
  pname = "vim-notebook";
  version = "707112f02d7774e2e841a60a8f75d83f0cafbec4";
  src = fetchFromGitHub {
    "owner" = "lifepillar";
    "repo" = "vim-solarized8";
    "rev" = "707112f02d7774e2e841a60a8f75d83f0cafbec4";
    "sha256" = "199ky54h53nldkcb31as3l9xmjs7l8hcck1dvbd6y252k5c5dqq1";
  };
}
