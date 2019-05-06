{pkgs}:

let
  tmuxline = pkgs.callPackage ../../packages/tmuxline {inherit pkgs;};
  ghcid = pkgs.callPackage ../../packages/ghcid.nix { inherit pkgs; };
  hsimport = pkgs.callPackage ../../packages/vim-hsimport.nix { inherit pkgs; };
  unite-haskellimport = pkgs.callPackage ../../packages/unite-haskellimport.nix { inherit pkgs; };
in
{
  enable = true;
  viAlias = true;
  vimAlias = true;
  withNodeJs = true;
  withPython = true;
  withPython3 = true;
  configure = with pkgs.vimPlugins; {
    customRC = pkgs.lib.readFile ./vimrc;
    plug.plugins = [
      ale
      ack-vim
      airline
      calendar
      calendar-vim
      colors-solarized
      denite-nvim
      deoplete-nvim
      fugitive
      gitgutter
      ##ghcmod-vim
      ghcid
      haskell-vim
      Hoogle
      hsimport
      LanguageClient-neovim
      ##intero-neovim
      ##neco-ghc
      #neoformat
      #neomake
      neomru
      nerdcommenter
      nerdtree
      psc-ide-vim
      purescript-vim
      supertab
      #syntastic
      tmuxline
      unite
      unite-haskellimport
      vim-airline-themes
      vim-better-whitespace
      vim-devicons
      vim-eunuch
      vim-hardtime
      vim-hdevtools
      vim-nix
      ##vim-searchhi
      vim-slime
      ##vim-tree
    ];
  };
}
