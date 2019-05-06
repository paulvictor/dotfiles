self: super:

{
  surfraw = super.surfraw.overrideDerivation(oldattrs: {
    preConfigure = '' sed -i 's/! -type l//' surfraw.IN; '';
  });
}
