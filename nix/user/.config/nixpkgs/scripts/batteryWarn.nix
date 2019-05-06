{ pkgs }:
with pkgs;
writeShellScriptBin "batteryWarn" ''
# TODO : Run it with systemd timers
while [ true ]
do
  bstatus=`${acpi}/bin/acpi -b`

  (echo $bstatus | ${gnugrep}/bin/grep -q Discharging) &&
    ( percent=`${acpi}/bin/acpi -b | ${gawk}/bin/awk -F ',' '{print $2}' | ${gnused}/bin/sed 's/%//'`; [ $percent -lt 10 ] && ${libnotify}/bin/notify-send -u critical -t 10000 $bstatus )
  sleep 30
done
''
