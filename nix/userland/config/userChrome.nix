{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  firefox-csshacks = fetchFromGitHub {
    owner = "MrOtherGuy";
    repo = "firefox-csshacks";
    rev = "8957a709e3abb7242fa89339c684f8027e66774c";
    sha256 = "sha256-O31wh4w/2OEbzkvUN+6lHYtLOTnz9Z/rHj659UzOtzg=";
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
    #"chrome/classic_firefox_menu_button.css"
    #"chrome/overlay_menubar.css"
#     "chrome/autohide_menubar.css"
    #"chrome/hide_tabs_toolbar_v2.css"
#     "chrome/hide_toolbox_top_bottom_borders.css"
#    "chrome/hide_urlbar_first_row.css"
#     "chrome/show_navbar_on_focus_only.css"
    #"chrome/tab_loading_progress_throbber.css"
    # "chrome/curved_tabs.css"
    # "chrome/toggle_tabs_toolbar_with_alt.css"
    #"chrome/linux_gtk_window_control_patch.css"
    # "chrome/tabs_below_content.css"
    #"chrome/show_navbar_on_focus_only.css"
    #"chrome/tabs_on_bottom_menubar_on_top_patch.css"
    #"chrome/dark_context_menus.css"
#     "chrome/compact_proton.css"
    #"chrome/theme_color_variables.css"
    #"chrome/tab_close_button_always_on_hover.css"
    #"chrome/round_ui_items.css"
    #"chrome/tabs_on_bottom_v2.css"
#     "chrome/autohide_bookmarks_and_main_toolbars.css"
    #"chrome/navbar_tabs_responsive_oneliner.css"
  ]) + "\n" +
# (lib.concatMapStringsSep "\n" (makeImportStatement userChrome-tweaks)
#   [ "hamburger/move-to-top-left-Linux-Windows.css"
#     "tabs/more-compact-tabs.css"
#     "tabs/material-design.css"
#     "context-menu/dark-thin-context-menu.css"
#   ])
# +
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


#urlbar-container,
#urlbar {
  visibility: collapse !important;
}

#navigator-toolbox{
  --uc-bm-padding: 4px; /* Vertical padding to be applied to bookmarks */
  --uc-bm-height: 0px;
  --uc-navbar-height: 0px;
}
#sidebar-box,
#sidebar-main,
#sidebar-splitter{
  visibility: collapse !important;
}

/* Disable elements  */
#context-navigation,
#context-savepage,
#context-pocket,
#context-sendpagetodevice,
#context-selectall,
#context-viewsource,
#context-inspect-a11y,
#context-sendlinktodevice,
#context-openlinkinusercontext-menu,
#context-bookmarklink,
#context-savelink,
#context-savelinktopocket,
#context-sendlinktodevice,
#context-searchselect,
#context-sendimage,
#context-print-selection {
  display: none !important;
}

#context_bookmarkTab,
#context_moveTabOptions,
#context_sendTabToDevice,
#context_reopenInContainer,
#context_selectAllTabs,
#context_closeTabOptions {
  display: none !important;
}

/* Toolbar  */
#tracking-protection-icon-container,
#urlbar-zoom-button,
#star-button-box,
#pageActionButton,
#pageActionSeparator,
#tabs-newtab-button,
#back-button,
#PanelUI-button,
#forward-button {
  display: none !important;
}

''
