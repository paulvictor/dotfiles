{ pkgs, lib, ... }:

{
  services.kanshi = {
    enable = lib.mkDefault false; # By default its false and its turned on per host
    systemdTarget = "sway-session.target"; # Ensures kanshi starts with Sway

    settings = [
      # 0. Mobile Profile: Laptop display ONLY
      {
        profile.name = "undocked";
        profile.outputs = [
          {
            criteria = "eDP-1";
            mode = "1920x1200";
            position = "0,0";
            status = "enable";
          }
        ];
      }

      # 1. Office Profile: External 4K monitor ONLY
      {
        profile.name = "office_ext_only";
        profile.outputs = [
          {
            criteria = "eDP-1"; # Usually your ThinkPad screen
            status = "disable";
          }
          {
            criteria = "DP-1"; # Replace with your actual 4K monitor name
            mode = "3840x2160@60Hz";
            position = "0,0";
            scale = 1.3; # Set to 1.5 or 2.0 if the UI is too small
            status = "enable";
          }
        ];
      }

      {
        profile.name = "office_mirrored";
        profile.outputs = [
          {
            criteria = "eDP-1";
            mode = "1920x1200";
            position = "0,0";
            status = "enable";
          }
          {
            criteria = "DP-1";
            mode = "3840x2160";
            position = "1920,0"; # Put it out of the way logically
            status = "enable";
          }
        ];
        # This command mirrors eDP-1 onto DP-1 in fullscreen
        profile.exec = "${pkgs.wl-mirror}/bin/wl-mirror eDP-1";
      }
      {
        profile.name = "office_stacked";
        profile.outputs = [
          {
            # External 4K Monitor (Top)
            criteria = "DP-1"; # Replace with your actual output name
            mode = "3840x2160@60Hz";
            position = "0,0";
            scale = 1.3;
            status = "enable";
          }
          {
            # ThinkPad Display (Bottom)
            criteria = "eDP-1";
            status = "enable";
            mode = "1920x1200";
            # 2160 / 1.5 = 1440.
            # We center it horizontally: (2560 - 1920) / 2 = 320
            position = "320,1440";
          }
        ];
      }


    ];
  };
}
