# The graphical environment on both sides: generic desktop services in NixOS, desktop apps and
# helpers in home-manager. Window manager is in `sway`, the browser in `firefox`.
{
  allowedUnfree = [
    "google-chrome" "vivaldi" "widevine-cdm"
    "ungoogled-chromium" "ungoogled-chromium-unwrapped"
  ];

  flake.modules.nixos.desktop = ./_nixos.nix;

  flake.modules.homeManager.desktop = { config, pkgs, lib, ... }: {
    home.packages = with pkgs; [
      acpi # TODO : Install only on laptops
      asciinema
      brightnessctl
      dunst
      adwaita-icon-theme
      vieb
      autorandr
      brotab
      hicolor-icon-theme
      league-of-moveable-type
      localsend
      material-icons
      mpv
      pdftk
      siji
      zathura # Crashing.
      nerd-fonts.sauce-code-pro # dunst (_dunstrc.nix)
    ]
    ++ lib.optionals pkgs.stdenv.hostPlatform.isx86_64 [ google-chrome ]
    ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [
      (pkgs.callPackage ./_menu-surfraw.nix { })
      scrot
      surfraw
      ungoogled-chromium
      vlc
    ];

    services.flameshot.enable = true;

    services.dunst = {
      enable = false;
      settings = import ./_dunstrc.nix;
      iconTheme = {
        name = "Numix";
        package = pkgs.numix-icon-theme;
        size = "48";
      };
    };

    xdg.configFile."zathura/zathurarc".text = ''
      set selection-clipboard clipboard
    '';
    home.file.".Xmodmap".text = builtins.readFile ./xmodmap;
    home.file.".urlview".text = ''
      SHORTCUT
      NOREVIEW
      RAW_RESERVED
    '';

    home.file.".vieb/colors/gruvbox.css".source = ./vieb/colors/gruvbox.css;
    home.file.".vieb/colors/smalltabs.css".source = ./vieb/colors/smalltabs.css;
    home.file.".vieb/colors/dark_minimal.css".source = ./vieb/colors/dark_minimal.css;
  };
}
