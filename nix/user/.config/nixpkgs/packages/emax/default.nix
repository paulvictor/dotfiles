{pkgs ? import <nixpkgs> {}}:

with pkgs;
let
  customizedEmacs =
  (emacsPackagesGen emacs).emacsWithPackages(epkgs:
  [ (with epkgs.melpaStablePackages;
      [
        magit
        evil
        centaur-tabs
        counsel
        find-file-in-project
        doom-themes
        doom-modeline
        all-the-icons
        linum-relative
        keyfreq
        ivy
        swiper
        projectile
        key-chord
        # avy
        ace-window
        envrc
        popup
        evil-collection
        perspective
        # ace-jump-mode
      ])
    ]
    ++
    [ (with epkgs.melpaPackages;
      [
        undo-fu
        nix-mode
        nix-modeline
        which-key
        fancy-dabbrev
        ess
        ess-R-data-view
        ess-smart-underscore
        vterm
        # elscreen
        # elscreen-separate-buffer-list
      ])
    ]);
    #++ [ (with epkgs.elpaPackages; [ undo-tree ]) ]);
  custom-init = import ./mk-init-el.nix { inherit pkgs; };
  myemacs = runCommand "myemacs" { buildInputs = [ makeWrapper ripgrep fd R ]; } ''
    mkdir -pv $out/bin
    makeWrapper ${customizedEmacs}/bin/emacs $out/bin/emax --add-flags -nl --add-flags --load --add-flags ${custom-init}
  '';
in
  pkgs.makeDesktopItem {
    name = "Emacs";
    exec = "${myemacs}/bin/emax %F";
    icon = "${pkgs.emacs}/share/icons/hicolor/scalable/apps/emacs.ico";
    desktopName = "Emacs";
  }
#mkShell {
#  nativeBuildInputs = [ makeWrapper ];
#  inputsFrom = [];
#  buildInputs = [
#    fd
#    ripgrep
#    myemacs
#  ];
#}
