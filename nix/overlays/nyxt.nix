final: prev:

let
  src = final.fetchFromGitHub {
    owner = "atlas-engineer";
    repo = "nyxt";
    rev = "aef5118a7c30c48583b464e2e0c27d4b0060999b";
    sha256 = "e83KmC9APjiYeK9ti+eejW4VZImZ36r4mVHf1RVe5aw=";
  };
  inherit (final.lispPackages_new) build-asdf-system sbcl sbclPackages;
#   nyxt_tests = build-asdf-system {
#     lisp = sbcl;
#     pname = "nyxt-tests";
#     version = "2022-09-22";

#     systems = [ "nyxt/tests" ];

#     inherit src;
#     lispLibs = with sbclPackages; [ nyxt nyxt-asdf prove ];
#   };

in
{
  nyxt-3 = build-asdf-system {
    lisp = sbcl;
    pname = "nyxt";
    version = "2022-09-22";

    inherit (sbclPackages.nyxt) nativeBuildInputs buildInputs buildScript installPhase;

#     asds = [ "nyxt" "nyxt-asdf" ];
#     systems = [ "nyxt" "nyxt/tests" ];

    lispLibs =
      sbclPackages.nyxt.lispLibs ++ # [ nyxt_tests ] ++
      (with sbclPackages; [ cl-cffi-gtk cl-webkit2 mk-string-metrics ]);

    inherit src;
  };
}
