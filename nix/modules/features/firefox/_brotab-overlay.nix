self: super:

{
  brotab = super.brotab.overrideAttrs(oldAttrs: {
    postInstall =
      ''
      sed -i "s|\$PWD/brotab_mediator.py|$out/bin/bt_mediator|" $out/lib/python*/site-packages/brotab/mediator/*json
      sed -i "s|\$PWD/brotab_mediator.py|$out/bin/bt_mediator|" $out/config/*json
      '';
  });
}
