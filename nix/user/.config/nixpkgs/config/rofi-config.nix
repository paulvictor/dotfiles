{ pkgs }:

with pkgs;
writeText "kitty.conf" ''
configuration {
/*	modi: "window,run,ssh";*/
  width: 50;
  lines: 15;
/*	columns: 1;*/
  font: "SauceCodePro Nerd Font 12";
/*	bw: 1;*/
/*	location: 0;*/
/*	padding: 5;*/
/*	yoffset: 0;*/
/*	xoffset: 0;*/
/*	fixed-num-lines: true;*/
  show-icons: true;
  terminal: "${pkgs.alacritty}/bin/alacritty";
/*	ssh-client: "ssh";*/
/*	ssh-command: "{terminal} -e {ssh-client} {host}";*/
/*	run-command: "{cmd}";*/
/*	run-list-command: "";*/
/*	run-shell-command: "{terminal} -e {cmd}";*/
/*	window-command: "xkill -id {window}";*/
/*	window-match-fields: "all";*/
/*	drun-icon-theme: ;*/
/*	drun-match-fields: "name,generic,exec,categories";*/
/*	disable-history: false;*/
/*	sort: false;*/
/*	levenshtein-sort: false;*/
/*	case-sensitive: false;*/
/*	cycle: true;*/
/*	sidebar-mode: false;*/
/*	eh: 1;*/
/*	auto-select: false;*/
/*	parse-hosts: false;*/
/*	parse-known-hosts: true;*/
/*	combi-modi: "window,run";*/
/*	matching: "normal";*/
/*	tokenize: true;*/
/*	m: "-5";*/
/*	line-margin: 2;*/
/*	line-padding: 1;*/
/*	filter: ;*/
/*	separator-style: "dash";*/
/*	hide-scrollbar: false;*/
/*	fullscreen: false;*/
/*	fake-transparency: false;*/
/*	dpi: -1;*/
/*	threads: 0;*/
/*	scrollbar-width: 8;*/
/*	scroll-method: 0;*/
/*	fake-background: "screenshot";*/
/*	window-format: "{w}    {i}{c}   {t}";*/
/*	click-to-exit: true;*/
/*	show-match: true;*/
        theme: "Monokai" ;
/*	color-normal: ;*/
/*	color-urgent: ;*/
/*	color-active: ;*/
/*	color-window: ;*/
/*	max-history-size: 25;*/
/*	combi-hide-mode-prefix: false;*/
/*	pid: "/run/user/979/rofi.pid";*/
/*	display-window: ;*/
/*	display-windowcd: ;*/
/*	display-run: ;*/
/*	display-ssh: ;*/
/*	display-drun: ;*/
/*	display-combi: ;*/
/*	display-keys: ;*/
/*	kb-primary-paste: "Control+V,Shift+Insert";*/
/*	kb-secondary-paste: "Control+v,Insert";*/
/*	kb-clear-line: "Control+w";*/
/*	kb-move-front: "Control+a";*/
/*	kb-move-end: "Control+e";*/
/*	kb-move-word-back: "Alt+b";*/
/*	kb-move-word-forward: "Alt+f";*/
/*	kb-move-char-back: "Left,Control+b";*/
/*	kb-move-char-forward: "Right,Control+f";*/
/*	kb-remove-word-back: "Control+Alt+h,Control+BackSpace";*/
/*	kb-remove-word-forward: "Control+Alt+d";*/
/*	kb-remove-char-forward: "Delete,Control+d";*/
/*	kb-remove-char-back: "BackSpace,Control+h";*/
/*	kb-remove-to-eol: "Control+k";*/
/*	kb-remove-to-sol: "Control+u";*/
/*	kb-accept-entry: "Control+j,Control+m,Return,KP_Enter";*/
/*	kb-accept-custom: "Control+Return";*/
/*	kb-accept-alt: "Shift+Return";*/
/*	kb-delete-entry: "Shift+Delete";*/
/*	kb-mode-next: "Shift+Right,Control+Tab";*/
/*	kb-mode-previous: "Shift+Left,Control+ISO_Left_Tab";*/
/*	kb-row-left: "Control+Page_Up";*/
/*	kb-row-right: "Control+Page_Down";*/
/*	kb-row-up: "Up,Control+p,ISO_Left_Tab";*/
/*	kb-row-down: "Down,Control+n";*/
/*	kb-row-tab: "Tab";*/
/*	kb-page-prev: "Page_Up";*/
/*	kb-page-next: "Page_Down";*/
/*	kb-row-first: "Home,KP_Home";*/
/*	kb-row-last: "End,KP_End";*/
/*	kb-row-select: "Control+space";*/
/*	kb-screenshot: "Alt+S";*/
/*	kb-toggle-case-sensitivity: "grave,dead_grave";*/
/*	kb-toggle-sort: "Alt+grave";*/
/*	kb-cancel: "Escape,Control+g,Control+bracketleft";*/
/*	kb-custom-1: "Alt+1";*/
/*	kb-custom-2: "Alt+2";*/
/*	kb-custom-3: "Alt+3";*/
/*	kb-custom-4: "Alt+4";*/
/*	kb-custom-5: "Alt+5";*/
/*	kb-custom-6: "Alt+6";*/
/*	kb-custom-7: "Alt+7";*/
/*	kb-custom-8: "Alt+8";*/
/*	kb-custom-9: "Alt+9";*/
/*	kb-custom-10: "Alt+0";*/
/*	kb-custom-11: "Alt+exclam";*/
/*	kb-custom-12: "Alt+at";*/
/*	kb-custom-13: "Alt+numbersign";*/
/*	kb-custom-14: "Alt+dollar";*/
/*	kb-custom-15: "Alt+percent";*/
/*	kb-custom-16: "Alt+dead_circumflex";*/
/*	kb-custom-17: "Alt+ampersand";*/
/*	kb-custom-18: "Alt+asterisk";*/
/*	kb-custom-19: "Alt+parenleft";*/
/*	kb-select-1: "Super+1";*/
/*	kb-select-2: "Super+2";*/
/*	kb-select-3: "Super+3";*/
/*	kb-select-4: "Super+4";*/
/*	kb-select-5: "Super+5";*/
/*	kb-select-6: "Super+6";*/
/*	kb-select-7: "Super+7";*/
/*	kb-select-8: "Super+8";*/
/*	kb-select-9: "Super+9";*/
/*	kb-select-10: "Super+0";*/
/*	ml-row-left: "ScrollLeft";*/
/*	ml-row-right: "ScrollRight";*/
/*	ml-row-up: "ScrollUp";*/
/*	ml-row-down: "ScrollDown";*/
/*	me-select-entry: "MousePrimary";*/
/*	me-accept-entry: "MouseDPrimary";*/
/*	me-accept-custom: "Control+MouseDPrimary";*/
}
''
