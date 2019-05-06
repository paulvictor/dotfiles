{ pkgs ? import <nixpkgs> {} }:
with pkgs;
writeShellScript "batteryWarn" ''
bstatus=`${acpi}/bin/acpi -b`

(echo $bstatus | ${gnugrep}/bin/grep -q Discharging) &&
  ( percent=`${acpi}/bin/acpi -b | ${gawk}/bin/awk -F ',' '{print $2}' | ${gnused}/bin/sed 's/%//'`; [ $percent -lt 20 ] && ${libnotify}/bin/notify-send -u critical -t 10000 "$bstatus" ) || true
''
