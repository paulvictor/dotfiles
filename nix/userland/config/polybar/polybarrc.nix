{ pkgs, config }:

let
  mail = pkgs.writeScript "mail" ''
    QUERY="tag:inbox"
    IMG="✉"
    #IMG="🖂"
    export NOTMUCH_CONFIG=${config.xdg.configHome}/notmuch/notmuchrc

    UNREAD=$(${pkgs.notmuch}/bin/notmuch count $QUERY and is:unread)
    if [ $1 = "count" ]; then
      echo $(${pkgs.notmuch}/bin/notmuch count $QUERY)
    elif [ $1 = "icon" ]; then
      if [ $UNREAD == "0" ]; then
          echo $IMG
      else
          echo ""
      fi
    elif [ $1 = "icon-unread" ]; then
      if [ $UNREAD != "0" ]; then
          echo $IMG
      else
          echo ""
      fi
    fi
  '';
  configFile = pkgs.writeText "polybarrc" ''
  [colors]
  black = #000
  white = #fff
  grey = #aaa
  darkgrey = #555
  red = #f00
  green = #0f0

  [bar/top]
  monitor = ''${env:MONITOR:}
  ;monitor = eDP-1
  width = 100%
  height = 37
  dpi = 96

  tray-position = right
  tray-detached = false
  tray-maxsize = 20
  tray-transparent = true
  ;wm-restack = i3
  ;override-redirect = true

  background = #00000000
  foreground = #ccffffff

  line-color = ''${bar/bottom.background}
  line-size = 2

  spacing = 2
  padding-right = 5
  module-margin = 4

  font-0 = NotoSans-Regular:size=10;0
  font-1 = unifont:size=10;0
  font-2 = FontAwesome5Free:size=10;0
  font-3 = MaterialIcons:size=10;0
  font-4 = MiscTermsyn:size=10:antialias=false;0
  font-5 = SauceCodeProNerdFont:size=10;0
  font-6 = Siji:pixelsize=10;1

  modules-left = i3
  modules-center = cpu memory
  modules-right = mailicon mailicon-unread mailcount wireless-network battery date

  [bar/bottom]
  monitor = ''${env:MONITOR:}
  ;monitor = eDP-1
  bottom = true
  width = 100%
  height = 27

  background = ''${bar/top.background}
  foreground = ''${bar/top.foreground}

  line-color = ''${bar/top.background}
  line-size = 2

  spacing = 3
  padding-right = 4
  module-margin-left = 0
  module-margin-right = 6

  font-0 = NotoSans-Regular:size=8;0
  font-1 = unifont:size=6;-3
  font-2 = FontAwesome:size=8;-2
  font-3 = NotoSans-Regular:size=8;-1
  font-4 = MaterialIcons:size=10;-1
  font-5 = Termsynu:size=8:antialias=false;0
  font-6 = SauceCodeProNerdFontRegular:size=10;0

  ;modules-left = i3
  ; modules-right = cpu memory

  [module/backlight]
  type = internal/xbacklight
  format = <ramp> <bar>

  ramp-0 = 
  ramp-1 = 
  ramp-2 = 

  bar-width = 10
  bar-indicator = |
  bar-indicator-font = 3
  bar-indicator-foreground = #ff
  bar-fill = ─
  bar-fill-font = 3
  bar-fill-foreground = #c9665e
  bar-empty = ─
  bar-empty-font = 3
  bar-empty-foreground = #44

  [module/battery]
  type = internal/battery
  full-at = 98
  poll-interval = 5

  format-charging = <animation-charging> <label-charging>
  format-discharging = <ramp-capacity> <label-discharging>
  format-full = <ramp-capacity> <label-full>

  ramp-capacity-0 = 
  ramp-capacity-0-foreground = #f53c3c
  ramp-capacity-1 = 
  ramp-capacity-1-foreground = #ffa900
  ramp-capacity-2 = 
  ramp-capacity-3 = 
  ramp-capacity-4 = 

  bar-capacity-width = 10
  bar-capacity-format = %{+u}%{+o}%fill%%empty%%{-u}%{-o}
  bar-capacity-fill = █
  bar-capacity-fill-foreground = #ddffffff
  bar-capacity-fill-font = 3
  bar-capacity-empty = █
  bar-capacity-empty-font = 3
  bar-capacity-empty-foreground = #44ffffff

  animation-charging-0 = 
  animation-charging-1 = 
  animation-charging-2 = 
  animation-charging-3 = 
  animation-charging-4 = 
  animation-charging-framerate = 750

  [module/bspwm]
  type = internal/bspwm

  ws-icon-0 = term;
  ws-icon-1 = web;
  ws-icon-2 = code;
  ws-icon-3 = music;
  ws-icon-4 = irssi;
  ws-icon-default = 

  format = <label-state> <label-mode>

  label-dimmed-underline = ''${BAR.background}

  label-active = %icon%
  label-active-foreground = #fff
  label-active-background = #773f3f3f
  label-active-underline = #c9665e
  label-active-font = 4
  label-active-padding = 4

  label-occupied = %icon%
  label-occupied-foreground = #dd
  label-occupied-underline = #666
  label-occupied-font = 4
  label-occupied-padding = 4

  label-urgent = %icon%
  label-urgent-foreground = #000000
  label-urgent-background = #bd2c40
  label-urgent-underline = #9b0a20
  label-urgent-font = 4
  label-urgent-padding = 4

  label-empty = %icon%
  label-empty-foreground = #55
  label-empty-font = 4
  label-empty-padding = 4

  label-monocle = 
  label-monocle-underline = ''${module/bspwm.label-active-underline}
  label-monocle-background = #33ffffff
  label-monocle-padding = 2

  label-locked = 
  label-locked-foreground = #bd2c40
  label-locked-underline = ''${module/bspwm.label-monocle-underline}
  label-locked-padding = ''${module/bspwm.label-monocle-padding}

  label-sticky = 
  label-sticky-foreground = #fba922
  label-sticky-underline = ''${module/bspwm.label-monocle-underline}
  label-sticky-padding = ''${module/bspwm.label-monocle-padding}

  label-private = 
  label-private-foreground = #bd2c40
  label-private-underline = ''${module/bspwm.label-monocle-underline}
  label-private-padding = ''${module/bspwm.label-monocle-padding}

  [module/bspwm-tmp]
  type = internal/bspwm
  format = <label-state>

  label-active = 
  label-active-padding = 1
  label-occupied = 
  label-occupied-padding = 1
  label-empty = 
  label-empty-padding = 1

  [module/cpu]
  type = internal/cpu
  interval = 2.0
  format = <label> <ramp-coreload>
  label = CPU %percentage%%

  ramp-coreload-0 = ▁
  ramp-coreload-0-font = 2
  ramp-coreload-0-foreground = #aaff77
  ramp-coreload-1 = ▂
  ramp-coreload-1-font = 2
  ramp-coreload-1-foreground = #aaff77
  ramp-coreload-2 = ▃
  ramp-coreload-2-font = 2
  ramp-coreload-2-foreground = #aaff77
  ramp-coreload-3 = ▄
  ramp-coreload-3-font = 2
  ramp-coreload-3-foreground = #aaff77
  ramp-coreload-4 = ▅
  ramp-coreload-4-font = 2
  ramp-coreload-4-foreground = #fba922
  ramp-coreload-5 = ▆
  ramp-coreload-5-font = 2
  ramp-coreload-5-foreground = #fba922
  ramp-coreload-6 = ▇
  ramp-coreload-6-font = 2
  ramp-coreload-6-foreground = #ff5555
  ramp-coreload-7 = █
  ramp-coreload-7-font = 2
  ramp-coreload-7-foreground = #ff5555

  [module/date]
  type = internal/date
  interval = 1.0
  ;date =   %d%%{F-}-%m-%%{F#99}%Y  %%{F#fff}%H:%M%%{F-}
  date =  %%{F#fff}%a, %d %b %Y  %%{F#fff}%H:%M%%{F#666}

  [module/memory]
  type = internal/memory
  format = <label> <bar-used>
  label = RAM: %gb_used% / %gb_total%

  bar-used-indicator =
  bar-used-width = 16
  bar-used-foreground-0 = #55aa55
  bar-used-foreground-1 = #557755
  bar-used-foreground-2 = #f5a70a
  bar-used-foreground-3 = #ff5555
  bar-used-fill = ▐
  bar-used-empty = ▐
  bar-used-empty-foreground = #444444


  [module/mpd]
  type = internal/mpd
  format-online = <icon-prev> <icon-stop> <toggle> <icon-next>  <icon-repeat> <icon-random>  <bar-progress> <label-time>  <label-song>

  icon-play = 
  icon-pause = 
  icon-stop = 
  icon-prev = 
  icon-next = 
  icon-random = 
  icon-repeat = 

  toggle-on-foreground =
  toggle-off-foreground = #55

  bar-progress-width = 45
  bar-progress-format = %{A4:mpdseek+2: A5:mpdseek-2:}%fill%%indicator%%empty%%{A A}
  bar-progress-indicator = |
  bar-progress-indicator-foreground = #ff
  bar-progress-indicator-font = 3
  bar-progress-fill = ─
  bar-progress-fill-foreground = #bb
  bar-progress-fill-font = 3
  bar-progress-empty = ─
  bar-progress-empty-font = 3
  bar-progress-empty-foreground = #44

  [module/wireless-network]
  type = internal/network
  interface = ''${env:WLIF:}
  interval = 3.0
  ping-interval = 10

  format-connected = <ramp-signal> <label-connected>
  label-connected = %essid%  ▴ %upspeed% ▾ %downspeed%   %signal%%
  label-disconnected =    not connected
  label-disconnected-foreground = #66

  ramp-signal-0 = 
  ramp-signal-1 = 
  ramp-signal-2 = 
  ramp-signal-3 = 
  ramp-signal-4 = 

  animation-packetloss-0 = 
  animation-packetloss-0-foreground = #ffa64c
  animation-packetloss-1 = 
  animation-packetloss-1-foreground = ''${bar/top.foreground}
  animation-packetloss-framerate = 500

  [module/wired-network]
  type = internal/network
  interface = net0
  interval = 3.0

  label-connected =    %{T3}%local_ip%%{T-}
  label-disconnected-foreground = #66

  [module/volume]
  type = internal/volume
  speaker-mixer = Speaker
  headphone-mixer = Headphone
  headphone-id = 9

  format-volume = <ramp-volume> <label-volume>
  label-muted =   muted
  label-muted-foreground = #66

  ramp-volume-0 = 
  ramp-volume-1 = 
  ramp-volume-2 = 
  ramp-volume-3 = 

  [module/powermenu]
  type = custom/menu

  format-padding = 5

  label-open = 
  label-close = 

  menu-0-0 = Terminate WM
  menu-0-0-foreground = #fba922
  menu-0-0-exec = bspc quit -1
  menu-0-1 = Reboot
  menu-0-1-foreground = #fba922
  menu-0-1-exec = menu_open-1
  menu-0-2 = Power off
  menu-0-2-foreground = #fba922
  menu-0-2-exec = menu_open-2

  menu-1-0 = Cancel
  menu-1-0-foreground = #fba922
  menu-1-0-exec = menu_open-0
  menu-1-1 = Reboot
  menu-1-1-foreground = #fba922
  menu-1-1-exec = sudo reboot

  menu-2-0 = Power off
  menu-2-0-foreground = #fba922
  menu-2-0-exec = sudo poweroff
  menu-2-1 = Cancel
  menu-2-1-foreground = #fba922
  menu-2-1-exec = menu_open-0

  [module/clock]
  type = internal/date
  interval = 2
  date = %%{F#999}%Y-%m-%d%%{F-}  %%{F#fff}%H:%M%%{F-}

  ; vim:ft=dosini
  ;
  [module/i3]
  type = internal/i3

  ; Only show workspaces defined on the same output as the bar
  ;
  ; Useful if you want to show monitor specific workspaces
  ; on different bars
  ;
  ; Default: false
  pin-workspaces = false

  ; This will split the workspace name on ':'
  ; Default: false
  strip-wsnumbers = true

  ; Sort the workspaces by index instead of the default
  ; sorting that groups the workspaces by output
  ; Default: false
  index-sort = true

  ; Create click handler used to focus workspace
  ; Default: true
  enable-click = false

  ; Create scroll handlers used to cycle workspaces
  ; Default: true
  enable-scroll = false

  ; Wrap around when reaching the first/last workspace
  ; Default: true
  wrapping-scroll = false

  ; Set the scroll cycle direction
  ; Default: true
  reverse-scroll = false

  ; Use fuzzy (partial) matching on labels when assigning
  ; icons to workspaces
  ; Example: code;♚ will apply the icon to all workspaces
  ; containing 'code' in the label
  ; Default: false
  fuzzy-match = true
  ; ws-icon-[0-9]+ = label;icon
  ; NOTE: The label needs to match the name of the i3 workspace
  ;workspaces = ["λ" "" "🗩" "" "♫" "⚙" "" "" "🗎" ""];
  ws-icon-0 = "1: code; "
  ws-icon-1 = "2: web; "
  ws-icon-2 = "3: messenger; "
  ws-icon-3 = "4: admin; "
  ws-icon-4 = "5: docs;"
  ws-icon-5 = "6: search;"
  ws-icon-6 = "7: scratch;"
  ws-icon-7 = "8: scratch;"
  ws-icon-8 = "9: scratch;"

  ; NOTE: You cannot skip icons, e.g. to get a ws-icon-6
  ; you must also define a ws-icon-5.

  ; Available tags:
  ;   <label-state> (default) - gets replaced with <label-(focused|unfocused|visible|urgent)>
  ;   <label-mode> (default)
  format = <label-state> <label-mode>

  ; Available tokens:
  ;   %mode%
  ; Default: %mode%
  label-mode = %mode%
  label-mode-padding = 2
  label-mode-background = #e60053

  ; Available tokens:
  ;   %name%
  ;   %icon%
  ;   %index%
  ;   %output%
  ; Default: %icon%  %name%
  label-focused = %index%  %icon%
  label-focused-foreground = #ffffff
  label-focused-background = #3f3f3f
  label-focused-underline = #fba922
  label-focused-padding = 4

  ; Available tokens:
  ;   %name%
  ;   %icon%
  ;   %index%
  ;   %output%
  ; Default: %icon%  %name%
  1abel-unfocused = %index%  %icon%  %name%
  label-unfocused-padding = 4

  ; Available tokens:
  ;   %name%
  ;   %icon%
  ;   %index%
  ;   %output%
  ; Default: %icon%  %name%
  label-visible = %index%  %icon%  %name%
  label-visible-underline = #555555
  label-visible-padding = 4

  ; Available tokens:
  ;   %name%
  ;   %icon%
  ;   %index%
  ;   %output%
  ; Default: %icon%  %name%
  label-urgent = %index%  %icon%  %name%
  label-urgent-foreground = #000000
  label-urgent-background = #bd2c40
  label-urgent-padding = 4

  ; Separator in between workspaces
  label-separator = |
  label-separator-padding = 2
  label-separator-foreground = #ffb52a

  [module/mailcount]
  type = custom/script
  interval = 5
  format-padding = 0
  ; label = %output:0:2%
  exec = ${mail} count

  [module/mailicon-unread]
  type = custom/script
  interval = 5
  format-foreground = ''${colors.red}
  exec = ${mail} icon-unread

  [module/mailicon]
  type = custom/script
  interval = 5
  ; format-foreground = ''${colors.foreground-alt}
  exec = ${mail} icon
  '';
in pkgs.lib.readFile configFile
