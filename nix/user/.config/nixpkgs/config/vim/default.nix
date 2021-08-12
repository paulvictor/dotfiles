{pkgs, recCallPkgs}:

let
  customPackages = recCallPkgs ../../packages/vim;
  wrapperArgs = {
    #withPython = true;
    withPython3 = true;
    withRuby = false;
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
    extraPython3Packages = (ps: [ ps.pynvim ]);
    #extraPythonPackages = (ps: [ ps.pynvim ]);
    configure = {
      customRC = pkgs.lib.readFile ./vimrc;
      plug.plugins =
        with customPackages;
        with pkgs.vimPlugins;
        [ (ale.overrideAttrs(oldAttrs: { patches = ./cabal-ghc.patch; }))
          #goyo
          limelight-vim
          ack-vim
          airline
          calendar
          coc-git
          colors-solarized
          denite-nvim
          deoplete-nvim
          dracula
          #fugitive
          #gitgutter
          ghcid
          haskell-vim
          Hoogle
          vim-hsimport
          julia-vim
          #LanguageClient-neovim
          #neoformat
          #neomake
          neomru
          NeoSolarized
          nerdcommenter
          nerdtree
          psc-ide-vim
          purescript-vim
          supertab
          syntastic
          tmuxline
          unite
          unite-haskellimport
          vimwiki
          vim-airline-themes
          vim-better-whitespace
          vim-devicons
          vim-eunuch
          vim-floaterm
          vim-hardtime
          vim-hdevtools
          vim-markdown
          vim-nix
          #vim-notebook
          ##vim-searchhi
          vim-solarized8
          vim-slime ];
    };
  };
in
pkgs.wrapNeovim pkgs.neovim wrapperArgs
