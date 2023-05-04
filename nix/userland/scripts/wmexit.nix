{ pkgs ? import <nixpkgs> {}}:
with pkgs;
writeShellScriptBin "wmexit" ''
  llock() {
    ${xsecurelock}/bin/xsecurelock
  }

  case "$1" in
    lock)
      llock
      ;;
    logout)
      s=$(${systemd}/bin/loginctl session-status | awk 'NR==1 {print $1}')
      ${systemd}/bin/loginctl terminate-session $s
      ;;
    suspend)
      systemctl suspend
      ;;
    hibernate)
      systemctl hibernate
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
