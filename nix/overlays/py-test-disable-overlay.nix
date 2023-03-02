self: super: {
  python310 = super.python39.override {
    packageOverrides = pyself: pysuper: {
      twisted = pysuper.twisted.overrideAttrs (_: {
        doCheck = false;
      });
      scipy = pysuper.scipy.overrideAttrs (_: {
        doCheck = false;
      });
    };
  };
}

