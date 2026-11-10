self: super:

{
  gpVpn = super.writeShellScriptBin "gp-vpn" ''
    cmd=''${1:-status}

    ssh_cmd() {
      ${super.sshpass}/bin/sshpass -p "" ${super.openssh}/bin/ssh gp-host "$@"
    }

    case $cmd in
      status)
        state=$(ssh_cmd systemctl is-active openconnect-gptun 2>/dev/null || echo "unreachable")
        case $state in
          active)
            echo '{"text":"GP","class":"connected","tooltip":"GlobalProtect VPN connected"}'
            ;;
          activating)
            echo '{"text":"GP","class":"connecting","tooltip":"GlobalProtect VPN connecting..."}'
            ;;
          *)
            echo '{"text":"GP","class":"disconnected","tooltip":"GlobalProtect VPN disconnected — click to reconnect"}'
            ;;
        esac
        ;;
      reconnect)
        ssh_cmd bash -c '
          systemctl restart openconnect-gptun
          timeout 30 bash -c "until systemctl is-active openconnect-gptun >/dev/null 2>&1; do sleep 2; done"
          systemctl restart dante
        '
        ;;
      disconnect)
        ssh_cmd systemctl stop openconnect-gptun
        ;;
    esac
  '';
}
