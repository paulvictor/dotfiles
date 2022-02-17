{ config, pkgs, lib, specialArgs, ... }:

with pkgs;
{
  services.mbsync = {
    enable = false;
    package = pkgs.writeShellScriptBin "mbsync" ''
       ${pkgs.isync}/bin/mbsync $@
       NOTMUCH_CONFIG=${config.xdg.configHome}/notmuch/notmuchrc ${pkgs.notmuch}/bin/notmuch new
    '';
  };
  programs.neomutt = {
    enable = true;
    checkStatsInterval = 20;
    sidebar = {
      enable = true;
    };
    sort = "threads";
    vimKeys = true;
    extraConfig = ''
      set sort_aux = reverse-last-date-received # Thread view
      set mark_old=no # new messages are always new untill they have been read.

      # Set preffered view modes
      auto_view text/html # text/plain text/calendar # view html automatically
      alternative_order text/plain text/enriched text/html
      source ${pkgs.neomutt}/share/doc/neomutt/colorschemes/solarized-dark-256.neomuttrc
      # Compose a new email (not a reply) to the sender
      bind index,pager @ compose-to-sender
      # We replace the date field '%{%b %d}', giving:
      set index_format='%4C %Z %<[y?%<[m?%<[d?%[%H:%M ]&%[%a %d]>&%[%b %d]>&%[%m/%y ]> %-15.15L (%?l?%4l&%4c?) %s'
      # Test  Date Range  Format String  Example
      # --------------------------------------------
      # %[d   Today       %[%H:%M ]      12:34
      # %[m   This month  %[%a %d]       Thu 12
      # %[y   This year   %[%b %d]       Dec 10
      #  —    Older       %[%m/%y ]      06/15

      set new_mail_command = "${pkgs.libnotify}/bin/notify-send --icon='${pkgs.neomutt}/share/doc/neomutt/logo/neomutt.svg' 'New Email' '%n new messages, %u unread.' &"

      bind index,pager <f1> sidebar-prev      # Previous Mailbox
      bind index,pager <f2> sidebar-next      # Next Mailbox
      bind index,pager \CP sidebar-prev       # Ctrl-Shift-P - Previous Mailbox
      bind index,pager \CN sidebar-next       # Ctrl-Shift-N - Next Mailbox
      bind index,pager \CO sidebar-open       # Ctrl-Shift-O - Open Highlighted Mailbox
      bind index,pager B sidebar-toggle-visible   # Use 'B' to switch the Sidebar on and off
      bind pager a group-reply                # Reply to all recipients
      bind attach <enter>    view-mailcap

      set edit_headers                     # show headers when composing
      set fast_reply                       # skip to compose when replying
      set askcc                            # ask for CC:
      set fcc_attach                       # save attachments with the body
      set forward_format = "Fwd: %s"       # format of subject when forwarding
      set forward_decode                   # decode when forwarding
      set attribution = "On %d, %n wrote:" # format of quoting header
      set reply_to                         # reply to Reply to: field
      set reverse_name                     # reply as whomever it was to
      set include                          # include message in replies
      set forward_quote                    # include message in forwards
      set text_flowed
      unset sig_dashes                     # no dashes before sig
      unset mime_forward                   # forward attachments as part of body

      unset confirmappend      # don't ask, just do!
      set quit                 # don't ask, just do!!
      unset mark_old           # read/new is good enough for me
      set beep_new             # bell on new mails
      set pipe_decode          # strip headers and eval mimes when piping
      set thorough_search      # strip headers and eval mimes before searching
      set timeout = 0

      set uncollapse_jump
      set sort_re
      set reply_regexp = "^(([Rr][Ee]?(\[[0-9]+\])?: *)?(\[[^]]+\] *)?)*"
      set quote_regexp = "^( {0,4}[>|:#%]| {0,4}[a-z0-9]+[>|]+)+"
      set send_charset = "utf-8:iso-8859-1:us-ascii"
      set charset = "utf-8"
      set arrow_cursor = "no" # Change `color indicator` depending
      set query_command="${pkgs.notmuch-addrlookup}/bin/notmuch-addrlookup --mutt '%s'"

      macro index <F5> \
        "<enter-command>set my_old_pipe_decode=\$pipe_decode my_old_wait_key=\$wait_key nopipe_decode nowait_key<enter>\
        <shell-escape>${pkgs.notmuch-mutt}/bin/notmuch-mutt -r --prompt search<enter>\
        <change-folder-readonly>`echo ''${XDG_CACHE_HOME:-$HOME/.cache}/notmuch/mutt/results`<enter>\
        <enter-command>set pipe_decode=\$my_old_pipe_decode wait_key=\$my_old_wait_key<enter>" \
              "notmuch: search mail"

      macro index \\\\ "<vfolder-from-query>" "Search mailbox"
      macro index A "<modify-labels>+archive -unread -inbox<enter>" "Archive message"
      macro index c "<change-vfolder>?" "Change to vfolder overview"
      macro index,pager gi "<change-folder>=INBOX<enter>"       "open inbox"
      macro index,pager gd "<change-folder>=Drafts<enter>"      "open drafts"
      macro index,pager gs "<change-folder>=Sent<enter>"        "open sent"
      macro index,pager V "<pipe-message>${extract_url}/bin/extract_url<enter> " "Get URLs from email"
    '';
  };
  programs.mbsync = {
    enable = true;
  };
  programs.notmuch = {
    enable = true;
    new.tags = [ "new" "unread" ];
    hooks.postNew =
      let
        notmuch-rules = pkgs.writeText "tagmails.nm" ''
          -new path:/Trash/
          -new path:/Sent/
          -new path:/Spam/
          +inbox path:/INBOX/
          +sent  from:paul.victor@juspay.in
          +to-me to:paul.victor@juspay.in and tag:new
          +meetings subject:(Invitation: paul.victor@juspay.in) and tag:new
          +meetings subject:(Canceled event paul.victor@juspay.in) and tag:new
          +docs subject:"Invitation to edit" and tag:new
          +ignored -new -inbox from:support@juspay.in and tag:new
          +ignored -new -inbox from:no-reply@juspay.in and tag:new
          +ignored -new -inbox to:analytics@juspay.in and tag:new
          +ignored -new -inbox to:monitor@juspay.in and tag:new
          +ooo -new -inbox subject:/OOO/ and tag:new
          -new tag:new
        '';
      in ''
        # Run on first import
        # notmuch tag -unread -- tag:unread -- to mark existing mails as not unread
        ${notmuch}/bin/notmuch tag --batch --input=${notmuch-rules}
      '';
  };
#   programs.alot = {
#     enable = true;
#   };
  programs.msmtp.enable = true;
  programs.astroid = {
    enable = true;
    externalEditor = "nvim  -- -c 'set ft=mail' '+set fileencoding=utf-8' '+set ff=unix' '+set enc=utf-8' '+set fo+=w' %1";
    extraConfig = {
      "astroid.hints.level" = -1;
    };
  };
  accounts.email.accounts = rec {
    "juspay.in" = {
      primary = true;
      #primary = false;
#       passwordCommand = "${pkgs.gnupg}/bin/gpg -dq .password-store/Juspay/Email/Mail/AllSpark-Dell.gpg"; # TODO Use address
      neomutt = {
        enable = true;
        extraConfig = ''
          set pgp_sign_as = 0xBFE9590DD8D67680
          set virtual_spoolfile=yes
          virtual-mailboxes \
            "To Me" "notmuch://?query=tag:to-me and not tag:ignored" \
            "Recent" "notmuch://?query=date:today or date:yesterday and not tag:ignored" \
            "Unread" "notmuch://?query=tag:unread and not tag:ignored" \
            "Sent" "notmuch://?query=tag:sent" \
            "Meetings" "notmuch://?query=tag:meetings" \
            "Docs" "notmuch://?query=tag:docs" \
            "OOO" "notmuch://?query=tag:ooo"
          set nm_exclude_tags = "junk-internal,analytics,deleted"
          set nm_unread_tag = unread
          '';
      };
      msmtp.enable = true;
      address = "paul.victor@juspay.in";
      realName = "Paul Victor Raj";
      flavor = "gmail.com";
      mbsync = {
        enable = false;
        create = "both";
        expunge = "both";
        extraConfig.account = {
          PipelineDepth = 50;
          Timeout = 60;
        };
        extraConfig.local = {
          MaxSize = "500k";
        };
        groups.main.channels = {
          inbox.extraConfig = {
            #MaxMessages = 2000;
            Patterns = "\"INBOX\"";
            ExpireUnread = "no";
          };
          drafts = {
            #masterPattern = "[Gmail]/Drafts";
            slavePattern = "Drafts";
            extraConfig = {
              Create = "Both";
              Expunge = "Both";
            };
          };
          sent = {
            extraConfig = {
              #MaxMessages = 2000;
              Create = "Both";
              Expunge = "Both";
            };
            #masterPattern = "[Gmail]/Sent Mail";
            slavePattern = "Sent";
          };
          starred= {
            #masterPattern = "\"[Gmail]/Starred\"";
            slavePattern = "Starred";
            extraConfig = {
              Create = "Both";
              Expunge = "Both";
            };
          };
        };
      };
      imap = {
        host = "imap.gmail.com";
        port = 993;
        tls = {
          enable = true;
        };
      };
      folders = {
        inbox = "INBOX";
        sent = "Sent";
        drafts = "Drafts";
        trash = "Trash";
      };
      signature = {
        showSignature = "append";
        text = ''
          Paul Victor Raj ( 0x18497AC961BB2FB6 )
          Sent from ${pkgs.neomutt}/bin/neomutt
        '';
      };
      notmuch.enable = true;
      imapnotify = {
        enable = true;
        boxes = [ "INBOX" ];
      };
      astroid = {
        enable = true;
      };
      gpg = {
        key = "3E6925C73B18D3DB43A2104EA96C9B89755DF7D2";
        signByDefault = true;
      };
    };
  };
  home.file.".mime.types".text = ''
    application/postscript          ps eps
    application/pgp                 pgp
    audio/x-aiff                    aif aifc aiff
    text/html                       html htm shtml
    image/gif                       gif;
    image/jpeg                      jpeg jpg;
  '';
  home.file.".mailcap".text = ''
    text/html; ${pkgs.w3m}/bin/w3m -I %{charset} -T text/html; copiousoutput; nametemplate=%s.html
    #text/html; ${pkgs.lynx}/bin/lynx -dump %s ; copiousoutput; nametemplate=%s.html; test=test -n "$DISPLAY"; needsterminal;
  '';
}
