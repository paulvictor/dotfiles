{pkgs }:

with pkgs;
writeShellScriptBin "launchPolybar" ''
# Terminate already running bar instances
  ${psmisc}/bin/killall -q polybar

# Wait until the processes have been shut down
  while ${procps}/bin/pgrep -u $UID -x polybar >/dev/null; do ${coreutils}/bin/sleep 1; done

  export WLIF=$(${coreutils}/bin/tail -n +3 /proc/net/wireless | ${gawk}/bin/gawk -F':' '{print $1}')
  echo $WLIF

  for m in $(${xorg.xrandr}/bin/xrandr --query | ${gnugrep}/bin/grep " connected" | ${coreutils}/bin/cut -d" " -f1); do
    MONITOR=$m polybar -l info --reload top &
  done

''
