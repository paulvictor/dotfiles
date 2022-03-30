{config, pkgs, lib,...}:

with pkgs;
{
  services.batteryAlert.enable = false;
  programs.autorandr = {
    enable = true;
    hooks.postswitch = {
      change-background = "${feh}/bin/feh --bg-scale ${wall1} ${wall2} ${wall3}";
    };
    profiles = {
        "two-monitor" = {
          fingerprint = {
            HDMI1 = "00ffffffffffff0009d12a80455400000320010380371f782e4455a9554d9d260f5054a56b80d1c0b300a9c08180810081c001010101565e00a0a0a029503020350029372100001a000000ff004a314e30313032303031510a20000000fd00324c1e5a19000a202020202020000000fc0042656e5120504432353030510a0115020323f150901f2221200514041312110302010706230907078301000065030c001000011d007251d01e206e28550029372100001e8c0ad08a20e02d10103e96002937210000188c0ad090204031200c4055002937210000188c0ad090204031200c4055002937210000180000000000000000000000000000000000000000f8";
            DP1 = "00ffffffffffff0009d12a804554000003200104a5371f783e4455a9554d9d260f5054256b80d1c0b300a9c08180810081c001010101565e00a0a0a029503020350029372100001a000000ff004a314e30313336313031510a20000000fd00324c1e5a19000a202020202020000000fc0042656e5120504432353030510a0157020323f150901f2221200514041312110302010706230907078301000065030c002000011d007251d01e206e28550029372100001e8c0ad08a20e02d10103e96002937210000188c0ad090204031200c4055002937210000188c0ad090204031200c4055002937210000180000000000000000000000000000000000000000e8";
          };
          config = {
            DP2.enable = false;
            HDMI2.enable = false;
            HDMI3.enable = false;
            VIRTUAL1.enable = false;
            HDMI1 = {
              enable = true;
              crtc = 0;
              primary = true;
              position = "0x618";
              mode = "2560x1440";
              rate = "59.95";
              rotate = "normal";
            };
            DP1 = {
              enable = true;
              crtc = 1;
              primary = false;
              position = "2560x0";
              mode = "2560x1440";
              rate = "59.95";
              rotate = "left";
            };
          };
        };
      };
  };
}
