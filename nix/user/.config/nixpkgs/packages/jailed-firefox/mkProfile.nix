{ pkgs }:

with pkgs;
writeText "firefox.profile" ''
  include ${firejail}/etc/firejail/firefox.profile
  ignore private-dev
  ignore nou2f
  ignore net none
  ignore nodbus
  ignore nosound
  ignore novideo
  ignore no3d
  ignore memory-deny-write-execute
  whitelist ''${HOME}/.mozilla
  whitelist ''${HOME}/Downloads
  whitelist ''${HOME}/.pulse
  whitelist ''${HOME}/.cache/mozilla/firefox
  whitelist ''${HOME}/.config/pulse
  whitelist ''${HOME}/.config/fontconfig
  whitelist ''${HOME}/.cache/fontconfig
  whitelist ''${HOME}/.cache
  whitelist ''${HOME}/.local/share/applications
  whitelist ''${HOME}/.config/mimeapps.list
  # Fonts get messed up when private-etc is added. #TODO : Investigate
  # private-etc adjtime,asound.conf,bashrc,binfmt.d,bluetooth,chromium,common,dbus-1,default,docker,fonts,fstab,fuse.conf,group,host.conf,hostid,hostname,hosts,inputrc,issue,kbd,ld.so.preload,libao.conf,lightdm,locale.conf,localtime,login.defs,lvm,machine-id,man_db.conf,modprobe.d,modules-load.d,mtab,nanorc,netgroup,NetworkManager,nix,nixos,NIXOS,nscd.conf,nsswitch.conf,openal,opt,os-release,pam.d,passwd,pki,polkit-1,profile,protocols,pulse,reader.conf,resolv.conf,resolvconf.conf,rpc,samba,services,set-environment,shells,ssh,ssl,static,subgid,subuid,sysctl.d,systemd,terminfo,tlp.conf,tmpfiles.d,udev,UPower,vconsole.conf,X11,zfs,zinputrc,zoneinfo,zprofile,zshenv,zshrc
  # private-etc hosts,passwd,mime.types,fonts,mailcap,firefox,xdg,gtk-3.0,X11,pulse,alternatives,localtime,nsswitch.conf,resolv.conf,ssl,group,polkit-1,tempfiles.d
  blacklist ''${HOME}/.ssh
  blacklist ''${HOME}/.gnupg
  ignore noexec
  ignore noexec ''${HOME}/.mozilla
  include ${firejail}/etc/firejail/firefox-common-addons.inc
  include ${firejail}/etc/firejail/allow-python3.inc
  tracelog
''

