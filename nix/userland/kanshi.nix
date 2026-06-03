{ pkgs, lib, specialArgs, ... }:

let
  # Determine the port name based on the hostname
  extPort = if specialArgs.hostname == "slash" then "DP-3" else "DP-1";

  # Helper for the stacked Y-position calculation
  # (4K logical height @ 1.3 scale is ~1660)
  # Set to 1.5 or 2.0 if the UI is too small
  extScale = 1.3;
  laptopY = 1660;
in
{
  services.kanshi = {
    enable = lib.mkDefault false; # By default its false and its turned on per host
    systemdTarget = "sway-session.target"; # Ensures kanshi starts with Sway

    settings = [
      # 1. MIRRORED (HQ)
      {
        profile.name = "mirror";
        profile.outputs = [
          { criteria = "eDP-1"; status = "enable"; position = "0,0"; }
          { criteria = extPort; status = "enable"; mode = "preferred"; position = "4000,0"; }
        ];
        profile.exec = "${pkgs.wl-mirror}/bin/wl-mirror -F ${extPort} eDP-1";
      }

      # 2. STACKED
      {
        profile.name = "office_stacked";
        profile.outputs = [
          {criteria = "eDP-1"; status = "enable"; position = "320,${toString laptopY}";}
          {criteria = extPort; status = "enable"; mode = "preferred"; position = "0,0"; scale = extScale;}
        ];
      }

      # 3. EXTERNAL ONLY
      {
        profile.name = "office_ext_only";
        profile.outputs = [
          { criteria = "eDP-1"; status = "disable"; }
          {criteria = extPort; status = "enable"; mode = "preferred"; scale = extScale;}
        ];
      }

      # 4. UNDOCKED (Fallback)
      {
        profile.name = "undocked";
        profile.outputs = [
          {criteria = "eDP-1"; status = "enable";}
        ];
      }
    ];
  };
}
