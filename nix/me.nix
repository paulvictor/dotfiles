{ config, pkgs, ... }:
{
  me = {
    username = "viktor";
    fullname = "Paul Victor Raj";
    email = "paulvictor@gmail.com";
    githubUsername = "paulvictor";
    sshKey = pkgs.fetchurl {
      url = "https://github.com/${config.me.githubUsername}.keys";
      hash = "sha256-Lr0PrPR+ePnXfp7ClNUSwSUd0g0vNFkuAEcry+DRtCc=";
    };
    gpgKey = null;
    hashedPassword = "$6$SCMbhhof$227ZIsJWgaZmuZX3gwWUTv4E5VrPaVKmZ/97cbU6yclJdn7To3F0ngRAcvmYX5mPOunW8bU6v16vqvxkqjivK.";
  };
}
