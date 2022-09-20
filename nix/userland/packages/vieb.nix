{ pkgs, config }:

let
  viebrc = pkgs.writeText "viebrc"
    ''
set adblocker=off
set cache=clearonquit
set commandhist=persistuseronly
set containercolors=^temp\d+~#ff0,^g$~orange,^w$~yellow,^f$~blue,^b$~green,^r$~purple,^l$~mediumslateblue
set containerkeeponreopen
set containernewtab=s:usecurrent
set containershowname=always
set containersplitpage=s:usecurrent
set containerstartuppage=main
set countlimit=100
set devtoolsposition=split
set downloadmethod=confirm
set downloadpath=${config.home.homeDirectory}/plain/Downloads
set favicons=session
set favoritepages=
" set firefoxmode=always
set follownewtabswitch
set guifontsize=15
set useragent=%default~%firefox
set guifullscreennavbar=oninput
set guifullscreentabbar=onupdate
set guihidetimeout=2000
" Refer https://github.com/Jelmerro/Vieb/issues/58
set guinavbar=oninput
set guitabbar=onupdate
set ignorecase
set incsearch
set keeprecentlyclosed
set mapsuggest=9000000000000000
set mapsuggestposition=topright
set maxmapdepth=10
set menupage=elementasneeded
set menuvieb=both
set mintabwidth=28
set modifiers=
set modifiers+=Alt
set modifiers+=Ctrl
set modifiers+=Meta
set modifiers+=Shift
"set mouse
"set mousenewtabswitch
"set mousevisualmode=onswitch
set nativenotification=always
set noclearcookiesonquit
set nocleardownloadsoncompleted
set nocleardownloadsonquit
set noclearhistoryonquit
set noclearlocalstorageonquit
set noclosablepinnedtabs
set notificationduration=6000
set notificationposition=bottomright

set permissioncamera=allow
" set permissionclipboardread=ask
" set permissionclosepage=allow
set permissiondisplaycapture=ask
set permissionfullscreen=allow
" set permissiongeolocation=ask
set permissionmediadevices=allowfull
set permissionmicrophone=allow
set permissionmidi=allow
set permissionmidisysex=allow
" set permissionnotifications=ask
" set permissionopenexternal=ask
" set permissionpersistentstorage=ask
" set permissionpointerlock=ask
" set permissionsallowed=
" set permissionsallowed+=discord.com~camera~microphone~mediadevices
" set permissionsallowed+=calendar.google.com~persistentstorage~notifications
" set permissionsallowed+=meet.google.com~persistentstorage~notifications~camera~microphone~mediadevices
" set permissionsallowed+=app.slack.com~microphone~camera~notifications
" set permissionsasked=
" set permissionsblocked=
" set permissionscreenwakelock=ask
" set permissionsensors=ask
" set permissionunknown=ask
" set permissionsallowed=^https?:\/\/meet\.google\/com.*~mediadevices~microphone~camera

set redirects=https?://(www\.)?google\.com(\.\w+)?/amp/s/amp\.(.*)~https://$3
set redirecttohttp
set requesttimeout=20000
set restoretabs
set restorewindowmaximize
set restorewindowposition
set restorewindowsize
set search=https://google.com/search?q=%s
" set search=https://duckduckgo.com/?kae=d&kav=1&ko=1&q=%s&ia=web
set searchwords=
set searchwords+=am~https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias=aps&field-keywords=%s
set searchwords+=gh~https://github.com/search?utf8=✓&q=%s
set searchwords+=g~https://www.google.com/search?q=%s
set searchwords+=w~https://en.wikipedia.org/wiki/Special:Search/%s
set searchwords+=y~https://www.youtube.com/results?search_query=%s
set showcmd
set smartcase
set spell
set spelllang=en-US,de-DE,ru,uk
set startuppages=
set storenewvisits=pages
set suggestcommands=9000000000000000
set suggestorder=
set suggestorder+=history
set suggestorder+=searchword
set suggestorder+=file
set suggesttopsites=0
set suspendonrestore=regular
set suspendtimeout=0
set tabcycle
set tabnexttocurrent
set taboverflow=scroll
set tabreopenposition=right
set timeout
set timeoutlen=1000
set vimcommand=nvim
set tabreopenposition=previous
set windowtitle=title

" Custom Commands
command! clipboard_yank <pageToClipboard>
command! fillcmdline_open <toExploreMode>
command! fillcmdline_tab <toCommandMode>buffer<Space>
command! tabfirst <:buffer 0>
command! tabfirst <:buffer 0>
command! tablast <:buffer 999>
command! tabprev <previousTab>
command! tabnext <nextTab>
command! tabmove_+1 <moveTabForward>
command! tabmove_-1 <moveTabBackward>
command! current_url_open <toExploreMode><End>
command! tabclosealltoleft <:lclose>
command! tabclosealltoright <:rclose>
command! mute_toggle <:mute>
command! stop <stopLoadingPage>
command! urlincrement_-1 <decreasePageNumber>
command! urlincrement_1 <increasePageNumber>
command! followpage_prev <previousPage>
command! followpage_next <nextPage>
command! urlparent <toParentUrl>
command! urlroot <toRootUrl>
command! zoom_1 <zoomReset>
command! zoom_0.1_true <zoomIn>
command! zoom_-0.1_true <zoomOut>
command! zoom_0.5_true 5<zoomIn>
command! zoom_-0.5_true 5<zoomOut>
command! zoom_0.5_true 5<zoomIn>
command! zoom_-0.5_true 5<zoomOut>
command! zoom_3 7<zoomIn>
command! zoom_0.3 7<zoomOut>

command! newTabExplore <openNewTab><toExploreMode>

nmap ; <toCommandMode>
nmap yy <:clipboard_yank>
nmap o <:fillcmdline_open>
nmap b <:fillcmdline_tab>
nmap g0 <:tabfirst>
nmap g^ <:tabfirst>
nmap g$ <:tablast>
nmap J <:tabprev>
nmap K <:tabnext>
nmap >> <:tabmove_+1>
nmap <lt><lt> <:tabmove_-1>
nmap O <:current_url_open>
nmap gx0 <:tabclosealltoleft>
nmap gx$ <:tabclosealltoright>
nmap <A-p> <:pin>
nmap <A-m> <:mute_toggle>
nmap x <:close>
nmap gf <startFollowCurrentTab>
nmap t <:newTabExplore>

nmap <C-x>- <:split>
nmap <C-x><Bslash> <:vsplit>
nmap <C-x>k <:close>

nmap <Space> <:set guitabbar=onupdate guinavbar=onupdate>
nmap <C-Space> <:set guitabbar=always guinavbar=always>
cmap <C-j> <commandHistoryPrevious>
cmap <C-k> <commandHistoryNext>
emap <C-j> <exploreHistoryPrevious>
emap <C-k> <exploreHistoryNext>

colorscheme dark_minimal
'';
in
pkgs.runCommand "vieb" { buildInputs = [ pkgs.makeWrapper ]; }
  ''
      mkdir $out
      # Link every top-level folder from pkgs.hello to our new target
      ln -s ${pkgs.vieb}/* $out
      # Except the bin folder
      rm $out/bin
      mkdir $out/bin
      # We create the bin folder ourselves and link every binary in it
      ln -s ${pkgs.vieb}/bin/* $out/bin
      # Except the hello binary
      rm $out/bin/vieb
      # Because we create this ourself, by creating a wrapper
      makeWrapper ${pkgs.vieb}/bin/vieb $out/bin/vieb \
        --add-flags "--datafolder=${config.home.homeDirectory}/plain/vieb" \
        --add-flags "--site-isolation=strict" \
        --add-flags "--config-file=${viebrc}"
    ''
