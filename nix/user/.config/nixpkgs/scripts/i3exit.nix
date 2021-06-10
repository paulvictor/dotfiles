{ pkgs }:
with pkgs;
writeShellScriptBin "i3exit" ''
  llock() {
    ${i3lock-fancy}/bin/i3lock-fancy -p -f Sauce-Code-Pro-Nerd-Font-Complete
  }

  case "$1" in
    lock)
      ${xorg.xset}/bin/xset -display :0.0 dpms force off
      llock
      ;;
    logout)
      ${xorg.xset}/bin/xset -display :0.0 dpms force off
      ${i3-gaps}/bin/i3-msg exit
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
