{pkgs ? import <nixpkgs> {}}:

with pkgs;
let
  customizedEmacs =
  (emacsPackagesGen emacs).emacsWithPackages(epkgs:
  [ (with epkgs.melpaPackages;
      [
        # ace-jump-mode
        ace-window
        all-the-icons
        all-the-icons-ivy
        all-the-icons-ivy-rich
        all-the-icons-dired
        all-the-icons-ibuffer
        anzu
        avy
        company
        company-prescient
        counsel
        counsel-projectile
        dash
        dashboard
        dired-single
        doom-modeline
        doom-themes
        # elscreen
        # elscreen-separate-buffer-list
        # eglot # Has some issue with project.el. Emacs 28 should fix this
        engine-mode
        envrc
        equake
        ess
        ess-R-data-view
        ess-smart-underscore
        evil
        evil-collection
        evil-org
        expand-region
        f
        git-gutter
        # fancy-dabbrev
        haskell-mode
        helpful
        ivy
        ivy-prescient
        ivy-rich
        key-chord
        keyfreq
        linum-relative
        lsp-mode
        magit
        nix-mode
        nix-modeline
        no-littering
        org-superstar
        origami
        page-break-lines
        paredit
        perspective
        persp-projectile
        popup
        prescient
        projectile
        rainbow-delimiters
        s
        slime
        slime-company
#         sly
#         sly-quicklisp
        swiper
        transient
        use-package
        use-package-chords
        # vimish-fold # Need keybindings but is good
        visual-fill-column
        vterm
        which-key
        wgrep
      ])
    ]
    ++
    [ (with epkgs.orgPackages; [ org ]) ]
    ++
    [ (with epkgs.elpaPackages; [ undo-tree ]) ]);
  myemacs = runCommand "myemacs" { buildInputs = [ makeWrapper ripgrep fd ]; } ''
    mkdir -pv $out/bin
    makeWrapper ${customizedEmacs}/bin/emacs $out/bin/emax \
      --prefix PATH : ${lib.makeBinPath [ ripgrep fd ]} \
      --add-flags --maximized
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
