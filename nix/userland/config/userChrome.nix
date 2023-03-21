{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  firefox-csshacks = fetchFromGitHub {
    owner = "MrOtherGuy";
    repo = "firefox-csshacks";
    rev = "b1b91c93d3f46cdc1013cebe20ef15dc506917af";
    sha256 = "0r27x1p2wiig9h626ak9nvym07shdfazp4b5h5bbm5ry0kwiwhlb";
    fetchSubmodules = true;
  };
  userChrome-tweaks = fetchFromGitHub {
    owner = "Timvde";
    repo = "UserChrome-Tweaks";
    rev = "5f972bcd8de94f037b16719db0fc9c1c97fca0d3";
    sha256 = "12zvysz7iby6pwhgax9aqmzq3wyvyrmk0aibx1imgnmkxz7cdam5";
   fetchSubmodules = true;
  };
  makeImportStatement = repo: relPath:
    ''@import url('file://${repo}/${relPath}');'';
in

(lib.concatMapStringsSep "\n" (makeImportStatement firefox-csshacks)
  [ #"chrome/tab_close_button_always_on_hover.css"
    "chrome/classic_firefox_menu_button.css"
    #"chrome/overlay_menubar.css"
    "chrome/autohide_menubar.css"
    "chrome/tab_loading_progress_throbber.css"
    # "chrome/curved_tabs.css"
    # "chrome/toggle_tabs_toolbar_with_alt.css"
    "chrome/linux_gtk_window_control_patch.css"
    # "chrome/tabs_below_content.css"
    "chrome/show_navbar_on_focus_only.css"
    #"chrome/tabs_on_bottom_menubar_on_top_patch.css"
    "chrome/dark_context_menus.css"
    "chrome/compact_proton.css"
    "chrome/theme_color_variables.css"
    "chrome/tab_close_button_always_on_hover.css"
    #"chrome/round_ui_items.css"
    #"chrome/tabs_on_bottom.css"
  ]) + "\n" +
(lib.concatMapStringsSep "\n" (makeImportStatement userChrome-tweaks)
  [ "hamburger/move-to-top-left-Linux-Windows.css"
    "tabs/more-compact-tabs.css"
    "tabs/material-design.css"
    "context-menu/dark-thin-context-menu.css"
  ])
+
''
span.TridactylHint {
    position: absolute;
    font-family: sans-serif;
    font-size: 12px;
    font-weight: bold;
    text-transform: uppercase;
    color: white;
    background-color: #204e8a;
    border-color: ButtonShadow;
    border-width: 0px;
    min-width: .75em;
    border-style: solid;
    padding: 0 1pt;
    text-align: center;
    z-index: 2147483647;
}

.TridactylHintElem, .TridactylHintActive {
    color: #ccc !important;
    animation: none;
    transition: unset;
}

.TridactylHintElem {
    background-color: rgba(13, 31, 54, 0.65);
    outline: 1px solid #204e8a;
}

.TridactylHintActive {
    background-color: #88FF00;
    outline: 1px solid #000;
}

div.TridactylHintHost {
    position: static !important;
}


/* :root{
   --toolbar-bgcolor: rgb(36,44,59) !important;
   --uc-menu-bkgnd: var(--toolbar-bgcolor);
   --arrowpanel-background: var(--toolbar-bgcolor) !important;
   --autocomplete-popup-background: var(--toolbar-bgcolor) !important;
   --uc-menu-disabled: rgb(90,90,90) !important;
   --lwt-toolbar-field-focus: rgb(36,44,59) !important;
 } */
/*#toolbar-menubar { visibility: collapse !important }
#urlbar-container { visibility: collapse !important }*/
/*
#nav-bar { visibility: collapse !important }
''
