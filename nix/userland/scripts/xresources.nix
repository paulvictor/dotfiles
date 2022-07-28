{ base16-builder, runCommandLocal, gnused, template, scheme, brightness }:

runCommandLocal "mk-xresources" { HOME = "$TMP"; }
  ''
    mkdir -pv $out
    ${base16-builder}/bin/base16-builder -s ${scheme} -t ${template} -b ${brightness} | ${gnused}/bin/sed '/background/s/#/[75]#/' > $out/config
  ''
