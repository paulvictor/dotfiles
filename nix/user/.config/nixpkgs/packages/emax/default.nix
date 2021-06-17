{pkgs ? import <nixpkgs> {}}:

with pkgs;
let
  customizedEmacs =
  (emacsPackagesGen emacs).emacsWithPackages(epkgs:
  [ (with epkgs.melpaStablePackages;
      [
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
        perspective
        # ace-jump-mode
      ])
    ]
    ++
    [ (with epkgs.orgPackages; [ org ]) ]
    ++
    [ (with epkgs.melpaPackages;
      [
        transient
        magit
        evil-collection
        undo-fu
        nix-mode
        nix-modeline
        which-key
        # fancy-dabbrev
        company
        ess
        ess-R-data-view
        ess-smart-underscore
        vterm
        haskell-mode
        evil-org
        rainbow-delimiters
        ivy-rich
        # elscreen
        # elscreen-separate-buffer-list
      ])
    ]);
    #++ [ (with epkgs.elpaPackages; [ undo-tree ]) ]);
  custom-init = import ./mk-init-el.nix { inherit pkgs; };
  myemacs = runCommand "myemacs" { buildInputs = [ makeWrapper ripgrep fd R ]; } ''
    mkdir -pv $out/bin
    makeWrapper ${customizedEmacs}/bin/emacs $out/bin/emax \
      --prefix PATH : ${lib.makeBinPath [ ripgrep fd ]} \
      --add-flags -nl \
      --add-flags --load \
      --add-flags ${custom-init}
  '';
in
{
  desktopApp = pkgs.makeDesktopItem {
    name = "Emacs";
    exec = "${myemacs}/bin/emax %F";
    icon = "${pkgs.emacs}/share/icons/hicolor/scalable/apps/emacs.ico";
    desktopName = "Emacs";
  };
  cliApp = myemacs;
}
