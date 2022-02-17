{ pkgs }:

with pkgs;
writeShellScriptBin "passdo" ''
  shopt -s nullglob globstar
  typeit=0
  notify=0
  otp_prefix=""
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

  password=$(printf '%s\n' "''${password_files[@]}" | ${rofi} -i -matching fuzzy -dmenu "$@")
  if echo $password | grep -q "TOTP$" ; then
    otp_prefix="otp"
  fi

  if [[ -n $password ]]; then
    if [[ $typeit -eq 1 ]]; then
      ${pass-with-extensions}/bin/pass $otp_prefix $password | tr -d '\n' | xdotool type --delay 100 --clearmodifiers --file -
      ${libnotify}/bin/notify-send "Typed password of $password"
    elif [[ $notify -eq 1 ]]; then
      pass=`${pass-with-extensions}/bin/pass $otp_prefix $password`
      ${libnotify}/bin/notify-send -t 45000 $password "<font size=16 color=blue><b><i>$pass</b></i></font>"
    else
      ${pass-with-extensions}/bin/pass -c $otp_prefix $password
    fi
  fi

''
