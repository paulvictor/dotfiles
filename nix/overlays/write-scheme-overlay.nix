final: _:

with final.writers;
{
  writeGuileScheme =
    { f ? "main", extraArgs ? "" }:
      makeScriptWriter {
        interpreter =
          ''
            ${guile}/bin/guile \
            -e ${f} ${extraArgs} -s
          '';
      };
}
