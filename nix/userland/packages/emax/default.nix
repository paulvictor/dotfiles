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
  agent-shell-bookmark = pkgs.emacsPackages.trivialBuild {
    pname = "agent-shell-bookmark";
    version = "0-unstable-2025-05-13";
    src = fetchFromGitHub {
      owner = "dcluna";
      repo = "agent-shell-bookmark";
      rev = "c1eab34bff4f35bf929885ed5045c6100afcf496";
      hash = "sha256-o9/QULEZ1rWAl0KBqIHf0yqHtBJCrqB4+Z1umJ7EFGM=";
    };
    packageRequires = [ pkgs.emacsPackages.melpaPackages.agent-shell ];
  };
  emacs-webkit-src = fetchFromGitHub {
    owner = "akirakyle";
    repo = "emacs-webkit";
    rev = "4c5caa8e2c2baa09400d3c4a467d4799d735d388";
    hash = "sha256-bHrfc9bGKY57+KGDRH5CdRflWH5va4jzGkMzXRrapg4=";
  };
  emacs-webkit = callPackage "${emacs-webkit-src}/default.nix" { inherit pkgs; };
  customizedEmacs =
    (emacsPackagesFor emacs-unstable).emacsWithPackages(epkgs:
        [ (with epkgs.melpaPackages;
          [
            ace-window
            agent-shell
            agent-shell-bookmark
            aidermacs
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
            gptel
            guix
            guru-mode
            haskell-mode
            helpful
            hide-mode-line
            himalaya
            hl-todo
            hydra
            iedit
            json-mode
            key-chord
            keyfreq
            kirigami
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
            nov
            nerd-icons-dired
            nix-mode
            nix-modeline
            nix-sandbox
            no-littering
            ob-bqn
            org-bullets
            org-beautify-theme
            org-download
            org-make-toc
            org-roam
            org-superstar
            org-tree-slide
            outline-indent
            origami # TODO not used
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
            # tree-sitter-langs
#             tree-sitter
            visual-fill-column
            vterm
            w3m
            wgrep
            which-key
            whole-line-or-region
            yaml-mode
            zerodark-theme
            zig-mode
            zoom-window
          ]
          ++ [ flim apel ] # Needed only from w3m atm
        )
        ]
        ++
        [ (with epkgs;
          [
            nano-theme
            eat
            eaf-browser
            eaf-pdf-viewer
            emacs-application-framework
            treesit-grammars.with-all-grammars
          ]) ]
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
      claude-agent-acp
      qwen-code
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
