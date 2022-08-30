{config, pkgs, lib,...}:

{
  services.batteryAlert.enable = true;
  programs.autorandr.enable = true;
  programs.autorandr.profiles = {
    readable = {
      fingerprint = {
        "eDP1" = "00ffffffffffff004c8329a0000000000a1d0104b5221378024481af503eb5230e505400000001010101010101010101010101010101fddf0030f2700c803020440058c21000001bfddf0030f2700c803020440058c21000001b000000fe00304848464d8131353657523034000000000003040300010000000b010a202001bf02030f00e3058000e6060501736dd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e2";
      };
      config = {
        eDP1 = {
          enable = true;
          crtc = 0;
          primary = true;
          position = "0x560";
          mode = "2560x1440";
          rate = "59.95";
        };
      };
    };
  };
}

