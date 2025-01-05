{pkgs, config, lib, ...}:

let
  gpg-dir = "${config.home.homeDirectory}/work-gpg"; # TODO make it a config
  passwordCommand =
    let
      app = pkgs.writeShellApplication {
        name = "juspay-password-cmd";
        text = "pass show work/Juspay/email-on-the-cli";
        runtimeEnv = {
          GNUPGHOME = gpg-dir;
        };
      };
    in "${app}/bin/juspay-password-cmd"; # lib.getExe' app "juspay-password-cmd";
in
{
  programs.notmuch = {
    enable = true;
    new.tags = [ "new" "unread" ];
    # hooks.postNew =
#       let
#         notmuch-rules = pkgs.writeText "tagmails.nm" ''
#           -new path:/Trash/
#           -new path:/Sent/
#           -new path:/Spam/
#           +inbox path:/INBOX/
#           +sent  from:paul.victor@juspay.in
#           +to-me to:paul.victor@juspay.in and tag:new
#           +meetings subject:(Invitation: paul.victor@juspay.in) and tag:new
#           +meetings subject:(Canceled event paul.victor@juspay.in) and tag:new
#           +docs subject:"Invitation to edit" and tag:new
#           +ignored -new -inbox from:support@juspay.in and tag:new
#           +ignored -new -inbox from:no-reply@juspay.in and tag:new
#           +ignored -new -inbox to:analytics@juspay.in and tag:new
#           +ignored -new -inbox to:monitor@juspay.in and tag:new
#           +ooo -new -inbox subject:/OOO/ and tag:new
#           -new tag:new
#         '';
#       in ''
#         # Run on first import
#         # notmuch tag -unread -- tag:unread -- to mark existing mails as not unread
#         ${config.programs.notmuch}/bin/notmuch tag --batch --input=${notmuch-rules}
#       '';
  };

  accounts.email.accounts = {
    "juspay.in" = {
      primary = true;
      address = "paul.victor@juspay.in";
      userName = "paul.victor@juspay.in";
      realName = "Paul Victor Raj";
      flavor = "gmail.com";
      inherit passwordCommand;
      imap = {
        host = "imap.gmail.com";
        port = 993;
        tls.enable = true;
      };
      smtp = {
        host = "smtp.gmail.com";
        port = 465;
        tls.enable = true;
      };

      maildir.path = "Juspay";

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
          Sent from ${lib.getExe config.programs.himalaya.package}
        '';
      };
      notmuch.enable = true;
      imapnotify = {
        enable = true;
        boxes = [ "INBOX" ];
      };
      himalaya.enable = true;
    };
  };
  programs.himalaya = {
    enable = true;
    package = pkgs.himalaya.override {
      buildFeatures = [
        "notmuch"
        "pgp-gpg"
      ];
    };
  };
  services.himalaya-watch = {
    enable = true;
    environment = {
      GNUPGHOME = gpg-dir;
    };
  };

}
