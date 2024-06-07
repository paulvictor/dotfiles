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
    if specialArgs.withGUI then emacs-unstable.override({withGTK3 = true; withImageMagick = true; }) else emacs-unstable-nox;
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
            cape
            company
            company-prescient
            copy-as-format
            dash
            dashboard
            dired-single
            direnv
            doom-modeline
            doom-themes
            edit-server
            elisp-slime-nav
            emacs-webkit
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
            hl-todo
            hydra
            iedit
            json-mode
            key-chord
            keyfreq
            linum-relative
            lispy
            lsp-haskell
            lsp-mode
            lsp-ui
            macrostep
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
            slack
            slime
            sly
            swiper
            transient
            use-package
            use-package-chords
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
    webkitgtk
    glib gtk3
    glib-networking
    gsettings-desktop-schemas
  ] ++ gstBuildInputs;

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
    ] ++ webkitDeps;
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
