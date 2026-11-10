{ pkgs, lib, emacsWithPackagesFromUsePackage, emacs-unstable, ... }:

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
  wasabi = pkgs.emacsPackages.trivialBuild {
    pname = "wasabi";
    version = "2026-07-01-unstable";
    src = pkgs.fetchFromGitHub {
      owner = "xenodium";
      repo = "wasabi";
      rev = "12bd301502df2e63de2049f99d9234b2039263b3";
      hash = "sha256-+RV445JnTZhOSUpEgdN4JfS9T3h9S2+9R+4phZ3ixD4=";
    };
    packageRequires = [ pkgs.emacsPackages.melpaPackages.acp ];
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
  lean4-mode = pkgs.emacsPackages.trivialBuild {
    pname = "lean4-mode";
    version = "0-unstable-2025-01-01";
    src = pkgs.fetchFromGitHub {
      owner = "leanprover-community";
      repo = "lean4-mode";
      rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
      hash = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
    };
    packageRequires = with pkgs.emacsPackages.melpaPackages; [ dash f s lsp-mode flycheck magit-section ];
    postInstall = ''
      mkdir -p $out/share/emacs/site-lisp/data
      cp $src/data/abbreviations.json $out/share/emacs/site-lisp/data/
    '';
  };
  emacs-webkit-src = fetchFromGitHub {
    owner = "akirakyle";
    repo = "emacs-webkit";
    rev = "4c5caa8e2c2baa09400d3c4a467d4799d735d388";
    hash = "sha256-bHrfc9bGKY57+KGDRH5CdRflWH5va4jzGkMzXRrapg4=";
  };
  emacs-webkit = callPackage "${emacs-webkit-src}/default.nix" { inherit pkgs; };
  customizedEmacs =
    emacsWithPackagesFromUsePackage {
      package = emacs-unstable;
      alwaysEnsure = true; # init files don't use :ensure t
      # A plain string (not a derivation) so this needs no IFD.
      # The parser can't handle non-ASCII, so keep these files ASCII-only.
      config = lib.concatMapStrings (f: builtins.readFile (./emacs.d + "/${f}"))
        [ "init.el" "completions.el" "eshell.el" "ai-coding.el" ];
      extraEmacsPackages = epkgs:
        [ (with epkgs.melpaPackages;
          [
            agent-shell-bookmark
            all-the-icons
            all-the-icons-dired
            all-the-icons-ibuffer
            anzu
            burly
            casual-suite
            clojure-mode
            cider
            copy-as-format
            dashboard
            edit-server
            elisp-slime-nav
            embark
            engine-mode
            erc-colorize erc-yank
            eshell-syntax-highlighting
            ess
            ess-R-data-view
            ess-smart-underscore
            flycheck
            geiser-chez
            geiser-guile
            general
            git-gutter
            guix
            guru-mode
            haskell-mode
            helpful
            hide-mode-line
            hl-todo
            json-mode
            key-chord
            keyfreq
            linum-relative
#             lsp-haskell
#             lsp-mode
#             lsp-ui
            macrostep
            macrostep-geiser
            nael-lsp
            nerd-icons-corfu
            nix-modeline
            nix-sandbox
            ob-bqn
            org-beautify-theme
            org-make-toc
            org-superstar
            origami # TODO not used
            page-break-lines
            paredit
            password-store
            perspective
            pilish
            popup
            prescient
            psci
            request
            ripgrep
            smartparens
            swiper
            transient
            # tree-sitter-langs
#             tree-sitter
            visual-fill-column
            vterm
            w3m
            wasabi
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
            eaf-browser
            eaf-pdf-viewer
            emacs-application-framework
            treesit-grammars.with-all-grammars
          ]) ]
        ++
        [ (with epkgs.elpaPackages; [ activities beframe undo-tree org vertico corfu plz kind-icon pulsar erc ement vundo tmr ]) ];
    };

  # Programs Emacs shells out to, put on its PATH
  runtimeDeps = [
    ripgrep
    fd
    w3m
    fish
    delta
    guile_3_0
    coreutils
    git
    wuzapi
    claude-agent-acp
    qwen-code
    inetutils
    gnupg
  ];

  myemacs = symlinkJoin {
    name = "Emacs";
    meta.mainProgram = "emacs";
    paths = [ customizedEmacs ];

    # GIO_EXTRA_MODULES = "${pkgs.glib-networking}/lib/gio/modules:${pkgs.dconf.lib}/lib/gio/modules";
    #   GST_PLUGIN_SYSTEM_PATH_1_0 = pkgs.lib.concatMapStringsSep ":" (p: "${p}/lib/gstreamer-1.0") gstBuildInputs;
    # --set GIO_EXTRA_MODULES "${pkgs.glib-networking}/lib/gio/modules:${pkgs.dconf.lib}/lib/gio/modules"
    nativeBuildInputs = [ makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/emacs \
        --prefix PATH : ${lib.makeBinPath runtimeDeps} \
        --add-flags --maximized
    '';
  };
in
myemacs
