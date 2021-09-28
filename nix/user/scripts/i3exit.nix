{ pkgs }:
with pkgs;
writeShellScriptBin "i3exit" ''
  llock() {
    gllock &
    sleep 3
  }

  case "$1" in
    lock)
      llock
      ${xorg.xset}/bin/xset -display :0.0 dpms force off
      ;;
    logout)
      local s=$(${systemd}/bin/loginctl session-status | awk 'NR==1 {print $1}')
      ${systemd}/bin/loginctl terminate-session $s
      ;;
    suspend)
      llock && systemctl suspend
      ;;
    hibernate)
      llock && systemctl hibernate
      ;;
    reboot)
      systemctl reboot
      ;;
    shutdown)
      systemctl poweroff
      ;;
    *)
      echo "Usage: $0 {lock|logout|suspend|hibernate|reboot|shutdown}"
      exit 2
  esac
  exit 0
''
