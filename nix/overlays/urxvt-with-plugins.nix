final: prev:

{
  rxvt-unicode = prev.rxvt-unicode.override {
    configure = { availablePlugins, ...}: {
      plugins = with availablePlugins; [
        perls
        vtwheel
        autocomplete-all-the-things
        resize-font
        tabbedex
      ];
    };
  };
}
