{ base16-builder, runCommandLocal, template, scheme, brightness }:

runCommandLocal "mk-xresources" { HOME = "$TMP"; }
  ''
    mkdir -pv $out
    ${base16-builder}/bin/base16-builder -s ${scheme} -t ${template} -b ${brightness} > $out/config
  ''
