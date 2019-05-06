{ pkgs }:

with pkgs;
with python37.pkgs;
buildPythonApplication rec {
  pname = "quickswitch-i3";
  version = "0f19f6473f38657f4122fd7f2b56565d193d044";
  propagatedBuildInputs = [ dmenu python3Packages.i3-py ];
  doCheck = false;
  src = fetchFromGitHub {
    owner = "OliverUv";
    repo = "quickswitch-for-i3";
    rev = "0f19f6473f38657f4122fd7f2b56565d193d044d";
    sha256 = "0n3g3gfm9jfbkk9vllla77zwzwyx9sfgq6xk47mwwag5wd6hp6n9";
  };
}
