{ pkgs }:

# From https://github.com/LumiGuide/lumi-example/blob/master/nix/lib.nix
with pkgs;
{
  ip4 = rec {
    ip = a : b : c : d : prefixLength : {
      inherit a b c d prefixLength;
      address = "${toString a}.${toString b}.${toString c}.${toString d}";
    };

    toCIDR = addr : "${addr.address}/${toString addr.prefixLength}";
    toNetworkAddress = addr : with addr; { inherit address prefixLength; };
    toNumber = addr : with addr; a * 16777216 + b * 65536 + c * 256 + d;
    fromNumber = addr : prefixLength :
      let
        aBlock = a * 16777216;
        bBlock = b * 65536;
        cBlock = c * 256;
        a      =  addr / 16777216;
        b      = (addr - aBlock) / 65536;
        c      = (addr - aBlock - bBlock) / 256;
        d      =  addr - aBlock - bBlock - cBlock;
      in
        ip a b c d prefixLength;

    fromString = with lib; str :
      let
        splits1 = splitString "." str;
        splits2 = flatten (map (x: splitString "/" x) splits1);

        e = i : toInt (builtins.elemAt splits2 i);
      in
        ip (e 0) (e 1) (e 2) (e 3) (e 4);

    fromIPString = str : prefixLength :
      fromString "${str}/${toString prefixLength}";

    network = addr :
      let
        pfl = addr.prefixLength;
        pow = n : i :
          if i == 1 then
            n
          else
            if i == 0 then
              1
            else
              n * pow n (i - 1);

        shiftAmount = pow 2 (32 - pfl);
      in
        fromNumber ((toNumber addr) / shiftAmount * shiftAmount) pfl;
  };
}
