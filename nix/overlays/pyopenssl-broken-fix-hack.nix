final: prev:

# https://github.com/NixOS/nixpkgs/issues/175875
let
  isSilicon = prev.stdenv.hostPlatform.isDarwin
    && prev.stdenv.hostPlatform.isAarch64;
in
if isSilicon
then {
  python3 = prev.python3.override {
    packageOverrides = python-final: python-prev: {
      pyopenssl = python-prev.pyopenssl.overrideAttrs (oldAttrs: {
        meta.broken = false;
      });
    };
  };
} else { }
