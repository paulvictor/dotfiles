{ config, lib, pkgs, ...}:

{

  imports = [ ./pipewire-extra-config.nix ];

  # rtkit is optional but recommended
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    systemWide = true;
    alsa.enable = true;
    #     alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;
#     config.pipewire = {
#       "context.properties" = {
#         "link.max-buffers" = 64;
#         "log.level" = 2; # https://docs.pipewire.org/page_daemon.html
#         "default.clock.rate" = 48000;
#         "default.clock.quantum" = 1024;
#         "default.clock.min-quantum" = 32;
#         "default.clock.max-quantum" = 4096;
#       };

#       "context.spa-libs" = {
#         "audio.convert.*" = "audioconvert/libspa-audioconvert";
#         "api.alsa.*"      = "alsa/libspa-alsa";
#         "api.v4l2.*"      = "v4l2/libspa-v4l2";
#         "api.libcamera.*" = "libcamera/libspa-libcamera";
#         "api.bluez5.*"    = "bluez5/libspa-bluez5";
#         "api.vulkan.*"    = "vulkan/libspa-vulkan";
#         "api.jack.*"      = "jack/libspa-jack";
#         "support.*"       = "support/libspa-support";
#       };
#     };
    wireplumber.enable = true;

  };
  environment.etc = {
    # See https://pipewire.pages.freedesktop.org/wireplumber/configuration/bluetooth.html about WH 100XM3
    "wireplumber/bluetooth.lua.d/51-bluez-config.lua".text = ''
    bluez_monitor.enabled = true;
    bluez_monitor.properties = {
      ["bluez5.enable-sbc-xq"] = true,
      ["bluez5.enable-msbc"] = true,
      ["bluez5.enable-hw-volume"] = true,
      ["bluez5.codecs"] = "[ ldac aptx_hd aptx sbc_xq sbc aac ]",
      ["bluez5.headset-roles"] = "[ hsp_hs hfp_hf ]",
      ["bluez5.a2dp.ldac.quality"] = "auto",
      ["bluez5.a2dp.aac.bitratemode"] = 5
    }
	'';
#       ["device.profile"] = "a2dp-sink"
#       ["bluez5.auto-connect"] = "[ hfp_hf hsp_hs a2dp_sink a2dp_source ]",
  };
}
