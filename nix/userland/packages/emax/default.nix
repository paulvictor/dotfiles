{pkgs ? import <nixpkgs> {}, specialArgs ? {withGUI = true;}}:

with pkgs;
let
  emacsPulsar = emacsPackages.trivialBuild {
    pname = "pulsar";
    src = fetchgit {
      "url" = "https://gitlab.com/protesilaos/pulsar.git";
      "rev" = "5ff2b816337c803053170dfd6d8038238d752dc0";
      "sha256" = "0qa05v1lzn6h3sajih3qmm4zvx92f4chc7g0zwa1w5b8mhdhnvkd";
    };
  };
  _emacs =
    if specialArgs.withGUI then pkgs.emacsUnstable else pkgs.emacsUnstable-nox;
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
            all-the-icons-ivy
            all-the-icons-ivy-rich
            anzu
            avy
            company
            company-prescient
            copy-as-format
            counsel
            counsel-projectile
            dash
            dashboard
            dired-single
            direnv
            doom-modeline
            doom-themes
            # eglot # Has some issue with project.el. Emacs 28 should fix this
            edit-server
            elisp-slime-nav
            emacsPulsar
            # elscreen
            # elscreen-separate-buffer-list
            engine-mode
            # envrc
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
            ivy
            ivy-prescient
            ivy-rich
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
            (magit-delta.overrideAttrs(old: {
              propagatedBuildInputs = old.propagatedBuildInputs ++ [ delta ];
              propagatedNativeBuildInputs = [ delta ];
              nativeBuildInputs = old.nativeBuildInputs ++ [ delta ];
            }))

            nix-mode
            nix-modeline
            no-littering
            org-make-toc
            org-roam
            org-superstar
            origami
            page-break-lines
            paredit
            pdf-tools
            perspective
            persp-projectile
            popper
            popup
            prescient
            projectile
            psci
            psc-ide
            rainbow-delimiters
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
            visual-fill-column
            vterm
            w3m
            wgrep
            which-key
            yaml-mode
            yequake
            zerodark-theme
            zoom-window
          ]
          ++ [ flim apel ] # Needed only from w3m atm
        )
        ]
        ++
        [ (with epkgs; [ nano-theme ]) ]
        ++
        [ (with epkgs.elpaPackages; [ undo-tree org ]) ]);
  myemacs = symlinkJoin {
    name = "Emacs";
    paths = [ customizedEmacs ];
    buildInputs = [ makeWrapper ripgrep fd w3m fish delta ];
    postBuild = ''
      wrapProgram $out/bin/emacs \
        --prefix PATH : ${lib.makeBinPath [ ripgrep fd w3m fish delta ]} \
        --add-flags --maximized
    '';
  };
in
myemacs
