{pkgs ? import <nixpkgs> {}, specialArgs ? {withGUI = true;}}:

with pkgs;
let
  emacs-webkit-src = fetchFromGitHub {
    owner = "akirakyle";
    repo = "emacs-webkit";
    rev = "4c5caa8e2c2baa09400d3c4a467d4799d735d388";
    hash = "sha256-bHrfc9bGKY57+KGDRH5CdRflWH5va4jzGkMzXRrapg4=";
  };
  emacs-webkit = callPackage "${emacs-webkit-src}/default.nix" { inherit pkgs; };
  _emacs =
    if specialArgs.withGUI then emacs-unstable else emacsUnstable-nox;
  customizedEmacs =
    (emacsPackagesFor (_emacs.override { withImageMagick = true; }))
      .emacsWithPackages(epkgs:
        [ (with epkgs.melpaPackages;
          [
            # ace-jump-mode
            ace-window
            all-the-icons
            all-the-icons-dired
            all-the-icons-ibuffer
#             all-the-icons-ivy
            all-the-icons-completion
#             all-the-icons-ivy-rich
            anzu
            avy
            cape
            company
            company-prescient
            copy-as-format
#             counsel
#             counsel-projectile
            dash
            dashboard
            dired-single
            direnv
            doom-modeline
            doom-themes
            # eglot # Has some issue with project.el. Emacs 28 should fix this
            edit-server
            elisp-slime-nav
            emacs-webkit
            org-tree-slide
            # elscreen
            # elscreen-separate-buffer-list
            engine-mode
            # envrc
            # equake
            #         esh-autosuggest
            eshell-prompt-extras
            eshell-syntax-highlighting
            ess
            ess-R-data-view
            ess-smart-underscore
            evil
            evil-collection
            evil-org
            expand-region
            f
            # fancy-dabbrev
            fish-completion
            flycheck
            geiser
            geiser-guile
            general
            git-gutter
            guix
            guru-mode
            haskell-mode
            helpful
            hl-todo
            hydra
#             ivy
#             ivy-prescient
#             ivy-rich
            json-mode
            key-chord
            keyfreq
            linum-relative
            lispy
            lispyville
            lsp-haskell
            lsp-mode
            lsp-ui
            macrostep
            magit
            nerd-icons
            nix-mode
            nix-modeline
            nix-sandbox
            no-littering
            org-make-toc
            org-roam
            org-superstar
            origami
            page-break-lines
            paredit
            password-store
            pcmpl-args
            pdf-tools
            perspective
#             persp-projectile
            popper
            popup
            prescient
#             projectile
            psci
            psc-ide
            rainbow-delimiters
            request
            ripgrep
            s
            slack
            slime
            slime-company
#             sly
            swiper
            transient
            use-package
            use-package-chords
            # vimish-fold # Need keybindings but is good
            embark orderless consult marginalia
            visual-fill-column
            vterm
            w3m
            wgrep
            which-key
            yaml-mode
            # yequake
            zerodark-theme
            zoom-window
          ]
          ++ [ flim apel ] # Needed only from w3m atm
        )
        ]
        ++
        [ (with epkgs; [ nano-theme ]) ]
        ++
        [ (with epkgs.elpaPackages; [ undo-tree org vertico corfu plz kind-icon pulsar ]) ]);
  myemacs = symlinkJoin {
    name = "Emacs";
    paths = [ customizedEmacs ];
    buildInputs = [
      makeWrapper
      ripgrep
      fd
      w3m
      fish
      delta
    ];
    postBuild = ''
      wrapProgram $out/bin/emacs \
        --prefix PATH : ${lib.makeBinPath [ ripgrep fd w3m fish delta guile_3_0 ]} \
        --add-flags --maximized
    '';
  };
in
myemacs
