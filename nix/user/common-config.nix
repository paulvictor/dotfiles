{pkgs, config, ... }:

with pkgs;
rec {
  # Let Home Manager install and manage itself.
  #programs.home-manager.enable = true;
  # Done
#  services.pCloudCC = {
#    enable = true;
#    loginId = "paulvictor@gmail.com";
#    mountPoint = "${config.home.homeDirectory}/pcloud";
#    package = pCloudCC;
#  };
#  systemd.user.paths.MountMusic = {
#    Unit = {
#      Description = "Bind mount Music from pCloud";
#      After = [ "pCloudCC.service" ];
#    };
#    Path = {
#      PathExists = "${config.home.homeDirectory}/pcloud/Music";
#    };
#  };
#  systemd.user.services.MountMusic = {
#    Install = {
#        WantedBy = [ "default.target" ];
#    };
#    Unit = {
#      Description = "Mount Music Directory";
#      After = [ "pCloudCC.service" ];
#    };
#    Service = {
#      ExecStart = "${bindfs}/bin/bindfs -d ${config.home.homeDirectory}/pcloud/Music ${config.home.homeDirectory}/Music";
#      Type = "simple";
#    };
#  };
#   systemd.user.services.xmodmap = {
#     Unit = {
#       Description = "Loads the XModMap Keymap";
#       After = [ "graphical-session.target" ];
#       Requires = [ "graphical-session.target" ];
#       # Wants = [ "display-manager.service" ];
#     };
#     Service = {
#       Environment="XAUTHORITY=${config.home.homeDirectory}/.Xauthority";
#       ExecStart = "${xorg.xmodmap}/bin/xmodmap -verbose ${config.home.homeDirectory}/.Xmodmap";
#       Type = "oneshot";
#       RemainAfterExit = "yes";
#     };
#   };
   ;
  #services.kbfs = {
    #enable = true;
    #extraFlags = [ "-label kbfs" "-mount-type normal" ];
  #};
  #services.keybase.enable = true;
  #services.kdeconnect.enable = true;
  #services.kdeconnect.indicator = true;
  # Done
  programs.autorandr = {
    enable = true;
    hooks.postswitch = {
      "notify-i3" = "${i3-gaps}/bin/i3-msg restart";
#       "reload-wp" = "${feh}/bin/feh --bg-scale ${wall1} ${wall2} ${wall3}";
      "reload-compton" = ''systemctl --user restart compton.service'';
      "restart-polybar" = "systemctl --user restart polybar.service";
      "restart-yubikey-touch-detector" = "systemctl --user restart yubikey-touch-detector.service";
    };
    profiles = {
      triple = {
        fingerprint = {
          DP1-8 = "00ffffffffffff0010acb9a04c4d45312f1b0104a53420783a0495a9554d9d26105054a54b00714f8180a940d1c0d100010101010101283c80a070b023403020360006442100001e000000ff00395433434d37424c31454d4c0a000000fc0044454c4c2055323431350a2020000000fd00313d1e5311000a20202020202001e802031cf14f9005040302071601141f12132021222309070783010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e96000644210000180000000000000000000000000000000000000000000000000000000c";
          HDMI1 = "00ffffffffffff0010acc2d0543130311b1c010380351e78eaad75a9544d9d260f5054a54b008100b300d100714fa9408180d1c00101565e00a0a0a02950302035000e282100001a000000ff003458305256383735313031540a000000fc0044454c4c205032343138440a20000000fd0031561d711c000a202020202020019102031bb15090050403020716010611121513141f2065030c001000023a801871382d40582c45000e282100001e011d8018711c1620582c25000e282100009ebf1600a08038134030203a000e282100001a7e3900a080381f4030203a000e282100001a00000000000000000000000000000000000000000000000000000000d8";
          DP1-1 = "00ffffffffffff0010acc1d0545742301b1c0104a5351e783aad75a9544d9d260f5054a54b008100b300d100714fa9408180d1c00101565e00a0a0a02950302035000e282100001a000000ff003458305256383735304257540a000000fc0044454c4c205032343138440a20000000fd0031561d711c010a20202020202001ad020315b15090050403020716010611121513141f20023a801871382d40582c45000e282100001e011d8018711c1620582c25000e282100009ebf1600a08038134030203a000e282100001a7e3900a080381f4030203a000e282100001a0000000000000000000000000000000000000000000000000000000000000000000062";
        };
        config = {
          "HDMI1" = {
            enable = true;
            mode = "2560x1440";
            position = "1200x560";
            rate = "60.0";
            rotate = "normal";
            primary = true;
          };
          "DP1-8" = {
            enable = true;
            mode = "1920x1200";
            position = "0x412";
            rate = "60.0";
            rotate = "right";
            primary = false;
          };
          "DP1-1" = {
            enable = true;
            mode = "2560x1440";
            position = "3760x0";
            rate = "60.0";
            rotate = "left";
            primary = false;
          };
        };
      };
      dual = {
        fingerprint = {
          DP1-8 = "00ffffffffffff0010acb9a04c4d45312f1b0104a53420783a0495a9554d9d26105054a54b00714f8180a940d1c0d100010101010101283c80a070b023403020360006442100001e000000ff00395433434d37424c31454d4c0a000000fc0044454c4c2055323431350a2020000000fd00313d1e5311000a20202020202001e802031cf14f9005040302071601141f12132021222309070783010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e96000644210000180000000000000000000000000000000000000000000000000000000c";
          HDMI1 = "00ffffffffffff0010ac604157313433181c010380351e78ea2585a9544ca2260a5054a54b00714f8180a9c0d1c00101010101010101023a801871382d40582c45000f282100001e000000ff00424d4d52354e320a2020202020000000fc0044454c4c205332343139484d0a000000fd00304b1e5311000a20202020202001b802032bf14f90050403020716010611121513141f23097f078301000065030c001000681a00000101304be62a4480a070382740302035000f282100001a011d8018711c1620582c25000f282100009e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f282100001800000000000000000000000034";
        };
        config = {
          "HDMI1" = {
            enable = true;
            mode = "1920x1080";
            position = "0x456";
            rate = "60.0";
            rotate = "normal";
            primary = true;
          };
          "DP1-8" = {
            enable = true;
            mode = "1920x1200";
            position = "1920x0";
            rate = "60.0";
            rotate = "right";
            primary = false;
          };
        };
      };
    };
  };

}
