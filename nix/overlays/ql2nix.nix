self: super:

let
  ql2nix-src = self.fetchFromGitHub {
    owner = "SquircleSpace";
    repo = "ql2nix";
    rev = "39cbd136b656c31717248b8527a55aeff5404cb6";
    sha256 = "OGIE+0ZYnd5u6+LQYW68ayMzyvgcssoO0FWsh4/HPe8=";
  };
  ql2Nix_ = import ql2nix-src { pkgs = self; };
in
  {
    inherit (ql2Nix_) ql2nix mkNixlispBundle;
  }
