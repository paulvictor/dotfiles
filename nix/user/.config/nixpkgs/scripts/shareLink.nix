{pkgs, config}:

with pkgs;
writeShellScriptBin "shareLink" ''
  function urlWithAccessToken(){
    accesstoken=$(pass pcloud/access_token)
    if [ $1 == "file" ];then
      pcloudCodeUrl="https://api.pcloud.com/getfilepublink?access_token=$accesstoken"
    elif [ $1 == "folder" ]; then
      pcloudCodeUrl="https://api.pcloud.com/gettreepublink?access_token=$accesstoken"
    else
      pcloudCodeUrl="https://api.pcloud.com/getpublinkdownload?access_token=$accesstoken"
    fi
  }
  cd ${config.home.homeDirectory}/pcloud;
  file=$(${findutils}/bin/find . | ${rofi}/bin/rofi -dmenu -theme onedark -i -p "Share: " | ${gnused}/bin/sed 's/^.//')
  [[ $file -eq "" ]] && exit 0;
  [[ -d $file ]] && urlWithAccessToken folder || urlWithAccessToken file
  ${curl}/bin/curl $pcloudCodeUrl --data-urlencode "path=$file"
  pcloudCode=$(${curl}/bin/curl $pcloudCodeUrl --data-urlencode "path=$file" | ${jq}/bin/jq .code)
  urlWithAccessToken download
  downloadResp=$(${curl}/bin/curl -vv "$pcloudCodeUrl&code=$pcloudCode&forcedownload=1")
  host=$(echo -n $downloadResp | ${jq}/bin/jq '.hosts[0]' |  ${coreutils}/bin/tr -d '"')
  path=$(echo -n $downloadResp | ${jq}/bin/jq '.path' | ${coreutils}/bin/tr -d '"')
  finalUrl="https://$host$path"
  echo -n $finalUrl | ${xsel}/bin/xsel -i -b
  echo -n $finalUrl | ${xsel}/bin/xsel -i -p
  ${libnotify}/bin/notify-send "Copied $finalUrl"
''
