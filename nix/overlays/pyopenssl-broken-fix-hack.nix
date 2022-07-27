# final: prev:

# https://github.com/NixOS/nixpkgs/issues/175875
_: { python3, lib, ... }: {
#   python3Packages = (python3.override {
#     packageOverrides = _: { urllib3, ... }: {
#       urllib3 = urllib3.overrideAttrs ({ passthru, ... }: {
#         passthru = passthru // {
#           optional-dependencies =
#             lib.filterAttrs (n: _v: n != "secure")
#             passthru.optional-dependencies;
#         };
#         propagatedBuildInputs = passthru.optional-dependencies.brotli;
#       });
#     };
#   }).pkgs;
}
# let
#   isSilicon = prev.stdenv.hostPlatform.isDarwin
#     && prev.stdenv.hostPlatform.isAarch64;
# in
# if isSilicon
# then {
#   python3 = prev.python3.override {
#     packageOverrides = python-final: python-prev: {
#       pyopenssl = python-prev.pyopenssl.overrideAttrs (oldAttrs: {
#         meta.broken = false;
#       });
#     };
#   };
# } else { }
