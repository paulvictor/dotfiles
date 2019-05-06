{ pkgs }: # ? import <nixpkgs> {}}:

let
  recCallPkgs = dir:
    let content = builtins.readDir dir; in
      builtins.listToAttrs
         (map (n: {name = n; value = pkgs.callPackage (dir + ("/" + n)) {}; })
         (builtins.filter (n: builtins.pathExists (dir + ("/" + n + "/default.nix")))
           (builtins.attrNames content)));
  customPackages = recCallPkgs ../../packages/vim;
in
  {
    enable = true;
    package = pkgs.neovim-nightly;
    extraConfig = pkgs.lib.readFile ./vimrc;
    #withPython = true;
    withPython3 = true;
    withRuby = false;
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
    #extraPython3Packages = (ps: [ ps.pynvim ]);
    #extraPythonPackages = (ps: [ ps.pynvim ]);
    plugins =
      with customPackages;
      with pkgs.vimPlugins;
        [ ale
          goyo-vim
          limelight-vim
          ack-vim
          vim-airline
          calendar-vim
          coc-git
          vim-colors-solarized
          denite-nvim
          deoplete-nvim
          dracula
          vim-fugitive
          vim-gitgutter
          ghcid
          haskell-vim
          #Hoogle
          vim-hsimport
          julia-vim
          #LanguageClient-neovim
          #neoformat
          #neomake
          neomru-vim
          NeoSolarized
          nerdcommenter
          nerdtree
          psc-ide-vim
          purescript-vim
          supertab
          syntastic
          tmuxline
          unite-vim
          #unite-haskellimport
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
  }
