{ pkgs ? import <nixpkgs> {} }:

with pkgs;
''
" See also https://gist.github.com/ynakao/0cf8b207b3fccdf233df4fd03b6e674c/raw/cc42677a0dc22312a8717df6db8d409539dcf8f9/tridactyl_config
" https://github.com/glacambre/.dotfiles/blob/2077603882c7bfe695dd7bc61b539650419598ac/default/.config/tridactyl/tridactylrc

set customthemes.custom span.TridactylHint { font-family: monospace !important; background: transparent !important; color: black !important; text-shadow: cyan -1px -1px 0px, cyan -1px 0px 0px, cyan -1px 1px 0px, cyan 1px -1px 0px, cyan 1px 0px 0px, cyan 1px 1px 0px, cyan 0px 1px 0px, cyan 0px -1px 0px !important; }
set theme custom
set findcase smart
set autofocus false
set hintdelay 100
set hintnames uniform
set hintchars arstneio

set hintfiltermode vimperator-reflow
set hintuppercase false
set incsearch true
set newtab https://google.com
set yankto both
set modeindicatorshowkeys true

" Git{Hub,Lab} git clone via SSH yank
bind yg composite js "git clone " + document.location.href.replace(/https?:\/\//,"git@").replace("/",":").replace(/$/,".git") | clipboard yank
" bind d composite tab #; tabclose #
bind d composite tabclose | tab #
bind x tabclose

" Only hint search results on Google and DDG
" bindurl www.google.com f hint -Jc .rc > div > a
" bindurl www.google.com F hint -Jbc .rc > div > a

bindurl ^https://duckduckgo.com f hint -Jc [class=result__a]
bindurl ^https://duckduckgo.com F hint -Jbc [class=result__a]

" Allow Ctrl-a to select all in the commandline
unbind --mode=ex <C-a>

" Allow Ctrl-c to copy in the commandline
unbind --mode=ex <C-c>

" Handy multiwindow/multitasking binds
bind gd tabdetach
bind gD composite tabduplicate; tabdetach

" Learn to use JK
" bind <F1> tabprev
" bind <F2> tabnext_gt

bind / fillcmdline find
bind ? fillcmdline find -?
bind n findnext 1
bind N findnext -1
bind ,<Space> nohlsearch

bind 0 fillcmdline_notrail tabopen https://pursuit.purescript.org/search?q=
bind = fillcmdline_notrail tabopen https://hoogle.haskell.org/?hoogle=
bind c fillcmdline_notrail tabopen https://bitbucket.org/juspay/
" bind <Space> fillcmdline_notrail

js tri.browserBg.runtime.getPlatformInfo().then(os=>{const editorcmd = os.os=="linux" ? "${alacritty}/bin/alacritty -e vim -t tridactyl-vim" : "auto"; tri.config.set("editorcmd", editorcmd)})

quickmark g https://mail.google.com/mail/u/0/#inbox
quickmark j https://mail.google.com/mail/u/1/#inbox
quickmark m https://meet.google.com/landing?authuser=1
colorscheme base16-gruvbox-dark-soft

unbind j
bind j scrollpx 0 100
unbind k
bind k scrollpx 0 -100
set smoothscroll true
set scrollduration 200
''
