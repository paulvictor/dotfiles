{ pkgs, config }:

let
  sshKeysLocation = "${config.home.homeDirectory}/.ssh/ec2keys";
in
with pkgs;
writeText "onAttachMonitor" ''
  ${xorg.xmodmap}/bin/xmodmap ${config.home.homeDirectory}/.Xmodmap
  ${feh}/bin/feh --bg-scale ${wall1} ${wall2} ${wall3}
  ${autorandr}/bin/autorandr triple
  # Disable ssh agent to try out gpg-agent
  #eval `${openssh}/bin/ssh-agent`
  #for i in ${config.home.homeDirectory}/.ssh/ec2keys/*
  #do
    #${openssh}/bin/ssh-add $i
  #done
''
