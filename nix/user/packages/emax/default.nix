{pkgs ? import <nixpkgs> {}}:

with pkgs;
let
  customizedEmacs =
    (emacsPackagesGen (emacs.override {withImageMagick = true; }))
      .emacsWithPackages(epkgs:
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
            copy-as-format
            dash
            dashboard
            dired-single
            doom-modeline
            doom-themes
            # elscreen
            # elscreen-separate-buffer-list
            # eglot # Has some issue with project.el. Emacs 28 should fix this
            elisp-slime-nav
            engine-mode
            envrc
            equake
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
            fish-completion
            flycheck
            general
            git-gutter
            guru-mode
            # fancy-dabbrev
            haskell-mode
            helpful
            hydra
            ivy
            ivy-prescient
            ivy-rich
            key-chord
            keyfreq
            linum-relative
            lispy
            lispyville
            lsp-mode
            magit
            nix-mode
            nix-modeline
            no-littering
            org-make-toc
            org-superstar
            origami
            page-break-lines
            paredit
            pdf-tools
            perspective
            persp-projectile
            popup
            popper
            prescient
            projectile
            psc-ide
            psci
            rainbow-delimiters
            ripgrep
            s
            slack
            slime
            slime-company
            sly
            swiper
            transient
            use-package
            use-package-chords
            # vimish-fold # Need keybindings but is good
            visual-fill-column
            vterm
            w3m
            which-key
            wgrep
            yequake
            zerodark-theme
            zoom-window
          ]
          ++ [ flim apel ] # Needed only from w3m atm
        )
        ]
        ++
        [ (with epkgs.orgPackages; [ org ]) ]
        ++
        [ (with epkgs; [ nano-theme ]) ]
        ++
        [ (with epkgs.elpaPackages; [ undo-tree ]) ]);
  myemacs = symlinkJoin {
    name = "Emacs";
    paths = [ customizedEmacs ];
    buildInputs = [ makeWrapper ripgrep fd w3m fish ];
    postBuild = ''
      wrapProgram $out/bin/emacs \
        --prefix PATH : ${lib.makeBinPath [ ripgrep fd w3m fish ]} \
        --add-flags --maximized
    '';
  };
in
myemacs
