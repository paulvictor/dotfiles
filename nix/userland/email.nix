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
