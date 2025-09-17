{pkgs ? import <nixpkgs> {}}:

with pkgs;
let
  ob-bqn =
    let
      src = "${pkgs.emacsPackages.bqn-mode.src}/extras";
      version = pkgs.emacsPackages.bqn-mode.version;
    in pkgs.emacsPackages.trivialBuild {
      pname = "ob-bqn";
      inherit src version;
      packageRequires = [ pkgs.emacsPackages.bqn-mode ];
    };
  emacs-webkit-src = fetchFromGitHub {
    owner = "akirakyle";
    repo = "emacs-webkit";
    rev = "4c5caa8e2c2baa09400d3c4a467d4799d735d388";
    hash = "sha256-bHrfc9bGKY57+KGDRH5CdRflWH5va4jzGkMzXRrapg4=";
  };
  emacs-webkit = callPackage "${emacs-webkit-src}/default.nix" { inherit pkgs; };
  himalaya-emacs =
    let
      version = "934e8f8741e3cfff577d7119eceb2cfdb7cff6f3";
      src = fetchFromGitHub {
        owner = "dantecatalfamo";
        repo = "himalaya-emacs";
        rev = version;
        hash = "sha256-JuWh6KdvPw5dJK86Ak87G/eFSKyggZViuTaNflpIlt8=";
      };
    in
      pkgs.emacsPackages.trivialBuild {
        pname = "ngnk-mode";
        inherit version src;
      };
  ngnk-mode =
    let
      version = "c2b6f3d98f566061369bd00a91124da4280c9398";
      src = fetchFromGitHub {
        owner = "gitonthescene";
        repo = "ngnk-mode";
        rev = version;
        hash = "sha256-CSjVKWhAdYC4JZyTx0B8dS7I99y+mnCdnmRe2/Pfyx0=";
      };
    in
      pkgs.emacsPackages.trivialBuild {
        pname = "ngnk-mode";
        inherit version src;
      };
  customizedEmacs =
    (emacsPackagesFor emacs-unstable) # We are getting rid of the nox package. If needed, run emacs -nw
      .emacsWithPackages(epkgs:
        [ (with epkgs.melpaPackages;
          [
            ace-window
            all-the-icons
            all-the-icons-completion
            all-the-icons-dired
            all-the-icons-ibuffer
            anzu
            avy
            avy-zap
            bqn-mode
            browse-at-remote
            bufler burly
            cape
            casual-suite
            clojure-mode
            cider
            copy-as-format
            dash
            dashboard
            direnv
            doom-modeline
            doom-themes
            edit-server
            elisp-slime-nav
            embark orderless consult marginalia
            engine-mode
            erc-colorize erc-yank
            eshell-prompt-extras
            eshell-syntax-highlighting
            ess
            ess-R-data-view
            ess-smart-underscore
            expand-region
            f
            fish-completion
            flycheck
            geiser
            geiser-chez
            geiser-guile
            general
            git-gutter
            guix
            guru-mode
            haskell-mode
            helpful
            hide-mode-line
            himalaya-emacs
            hl-todo
            hydra
            iedit
            json-mode
            key-chord
            keyfreq
            linum-relative
            lsp-haskell
            lsp-mode
            lsp-ui
            macrostep
            macrostep-geiser
            magit
            nerd-icons
            nerd-icons-completion
            nerd-icons-corfu
            ngnk-mode
            nov
            nerd-icons-dired
            nix-mode
            nix-modeline
            nix-sandbox
            no-littering
#             ob-bqn
            org-bullets
            org-beautify-theme
            org-download
            org-make-toc
            org-roam
            org-superstar
            org-tree-slide
            origami
            page-break-lines
            paredit
            password-store
            pcmpl-args
            pdf-tools
            perspective
            popper
            popup
            prescient
            psci
            psc-ide
            rainbow-delimiters
            request
            ripgrep
            s
            smartparens
            slack
            sly
            swiper
            transient
            visual-fill-column
            vterm
            w3m
            wgrep
            which-key
            whole-line-or-region
            yaml-mode
            zerodark-theme
            zoom-window
          ]
          ++ [ flim apel ] # Needed only from w3m atm
        )
        ]
        ++
        [ (with epkgs; [ nano-theme eat ]) ]
        ++
        [ (with epkgs.elpaPackages; [ activities beframe undo-tree org vertico corfu plz kind-icon pulsar erc ement vundo tmr ]) ]);

  treeSitters = with pkgs.tree-sitter-grammars; [
    tree-sitter-nix
    tree-sitter-json
    tree-sitter-haskell
    tree-sitter-clojure
    tree-sitter-commonlisp
    tree-sitter-scheme
    tree-sitter-elisp
    tree-sitter-bash
  ];

  myemacs = symlinkJoin {
    name = "Emacs";
    paths = [ customizedEmacs ];

    # GIO_EXTRA_MODULES = "${pkgs.glib-networking}/lib/gio/modules:${pkgs.dconf.lib}/lib/gio/modules";
#   GST_PLUGIN_SYSTEM_PATH_1_0 = pkgs.lib.concatMapStringsSep ":" (p: "${p}/lib/gstreamer-1.0") gstBuildInputs;
    buildInputs = [
      makeWrapper
      ripgrep
      fd
      w3m
      fish
      delta
    ] ++ treeSitters;
    postBuild = ''
      wrapProgram $out/bin/emacs \
        --prefix PATH : ${lib.makeBinPath [ ripgrep fd w3m fish delta guile_3_0 coreutils git ]} \
        --set GIO_EXTRA_MODULES "${pkgs.glib-networking}/lib/gio/modules:${pkgs.dconf.lib}/lib/gio/modules" \
        --add-flags --maximized
    '';
  };
in
myemacs
