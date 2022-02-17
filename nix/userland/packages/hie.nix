{ pkgs }:
with pkgs;
import (fetchFromGitHub {
  owner = "domenkozar";
  repo = "hie-nix";
  rev = "3568848019da43c4581e931fcb7a6cb8b0f33079";
  sha256 = "00zs610p56l6afbizx2xbhc3wsr5v3fnwiwcs4hzk7myyzl2k4qc";
}) {}
