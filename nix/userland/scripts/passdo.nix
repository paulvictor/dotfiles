final: prev:

{
  passdo =
    final.lib.writeGuileScheme
      {f = "main";}
      "/bin/passdo"
      (builtins.readFile ./passdo.scm);
}
# { pkgs }:

# with pkgs;
# writeShellScriptBin "passdo" ''
#   shopt -s nullglob globstar
#   typeit=0
#   notify=0
#   otp_prefix=""
#   if [[ $1 == "--type" ]]; then
#     typeit=1
#     shift
#   elif [[ $1 == "--notify" ]]; then
#     notify=1
#     shift
#   fi

#   prefix=''${PASSWORD_STORE_DIR-~/.password-store}
#   password_files=( "$prefix"/**/*.gpg )
#   password_files=( "''${password_files[@]#"$prefix"/}" )
#   password_files=( "''${password_files[@]%.gpg}" )

#   password=$(printf '%s\n' "''${password_files[@]}" | ${rofi}/bin/rofi -i -matching fuzzy -dmenu "$@")
#   if echo $password | grep -q "TOTP$" ; then
#     otp_prefix="otp"
#   fi

#   if [[ -n $password ]]; then
#     if [[ $typeit -eq 1 ]]; then
#       pass $otp_prefix $password | tr -d '\n' | xdotool type --delay 40 --clearmodifiers --file -
#       ${libnotify}/bin/notify-send "Typed password of $password"
#     else
#       pass -c $otp_prefix $password
#     fi
#   fi
# ''
