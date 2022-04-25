{ config, lib, pkgs, ...}:

{
  # rtkit is optional but recommended
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    #     alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;
    config.pipewire = {
      "context.properties" = {
        "link.max-buffers" = 64;
        "log.level" = 2; # https://docs.pipewire.org/page_daemon.html
        "default.clock.rate" = 48000;
        "default.clock.quantum" = 1024;
        "default.clock.min-quantum" = 32;
        "default.clock.max-quantum" = 4096;
      };

      "context.spa-libs" = {
        "audio.convert.*" = "audioconvert/libspa-audioconvert";
        "api.alsa.*"      = "alsa/libspa-alsa";
        "api.v4l2.*"      = "v4l2/libspa-v4l2";
        "api.libcamera.*" = "libcamera/libspa-libcamera";
        "api.bluez5.*"    = "bluez5/libspa-bluez5";
        "api.vulkan.*"    = "vulkan/libspa-vulkan";
        "api.jack.*"      = "jack/libspa-jack";
        "support.*"       = "support/libspa-support";
      };
    };

  };
  environment.etc = {
    "wireplumber/bluetooth.lua.d/51-bluez-config.lua".text = ''
    bluez_monitor.properties = {
      ["bluez5.enable-sbc-xq"] = true,
      ["bluez5.enable-msbc"] = true,
      ["bluez5.enable-hw-volume"] = true,
      ["bluez5.codecs"] = "[ ldac sbc_xq sbc ]",
      ["bluez5.headset-roles"] = "[ a2dp_sink hsp_hs hsp_ag hfp_hf hfp_ag ]"
    }
	'';
  };
}
