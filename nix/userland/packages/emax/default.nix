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
  _emacs =
    if specialArgs.withGUI then emacs-unstable else emacs-unstable-nox;
  #
#     if specialArgs.withGUI then emacs-unstable.override({withGTK3 = true; withImageMagick = true; }) else emacs-unstable-nox;
  customizedEmacs =
    (emacsPackagesFor _emacs)
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
            browse-at-remote
            cape
            clojure-mode
            cider
            copy-as-format
            dash
            dashboard
#             dired-single
            direnv
            doom-modeline
            doom-themes
            edit-server
            elisp-slime-nav
#             emacs-webkit
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
            slime
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
        [ (with epkgs; [ nano-theme ]) ]
        ++
        [ (with epkgs.elpaPackages; [ undo-tree org vertico corfu plz kind-icon pulsar erc ]) ]);
  gstBuildInputs = with gst_all_1; [
    gstreamer gst-libav
    gst-plugins-base
    gst-plugins-good
    gst-plugins-bad
    gst-plugins-ugly
  ];
  webkitDeps = with pkgs; [
    webkitgtk_4_1
    glib gtk3
    glib-networking
    gsettings-desktop-schemas
  ] ++ gstBuildInputs;
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
    ] ++ webkitDeps ++ treeSitters;
    postBuild = ''
      wrapProgram $out/bin/emacs \
        --prefix PATH : ${lib.makeBinPath [ ripgrep fd w3m fish delta guile_3_0 ]} \
        --set GIO_EXTRA_MODULES "${pkgs.glib-networking}/lib/gio/modules:${pkgs.dconf.lib}/lib/gio/modules" \
        --set GST_PLUGIN_SYSTEM_PATH_1_0 "${pkgs.lib.concatMapStringsSep ":" (p: "${p}/lib/gstreamer-1.0") gstBuildInputs}" \
        --add-flags --maximized
    '';
  };
in
myemacs
