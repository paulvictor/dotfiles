{ pkgs }:

with pkgs;
writeShellScriptBin "passdo" ''
  shopt -s nullglob globstar
  typeit=0
  notify=0
  if [[ $1 == "--type" ]]; then
    typeit=1
    shift
  elif [[ $1 == "--notify" ]]; then
    notify=1
    shift
  fi

  prefix=''${PASSWORD_STORE_DIR-~/.password-store}
  password_files=( "$prefix"/**/*.gpg )
  password_files=( "''${password_files[@]#"$prefix"/}" )
  password_files=( "''${password_files[@]%.gpg}" )

  password=$(printf '%s\n' "''${password_files[@]}" | rofi -i -matching fuzzy -dmenu "$@")

  if [[ -n $password ]]; then
    if [[ $typeit -eq 1 ]]; then
      ${pass}/bin/pass $password | tr -d '\n' | xdotool type --delay 200 --clearmodifiers --file -
      ${libnotify}/bin/notify-send "Typed password of $password"
    elif [[ $notify -eq 1 ]]; then
      pass=`${pass}/bin/pass $password`
      ${libnotify}/bin/notify-send -t 45000 $password "<font size=16 color=blue><b><i>$pass</b></i></font>"
    else
      ${pass}/bin/pass -c $password
    fi
  fi

''
