{ pkgs, name, searchUrl, baseUrl}:

with pkgs;
writeShellScriptBin name ''
# elvis: ${name}
. surfraw || exit 1

w3_usage_hook () {
    cat <<EOF
Usage: $w3_argv0 [options] [search words]...
Description:
  Search ${name} for documentation
EOF
    w3_global_usage
}

w3_config
w3_parse_args "$@"
if test -z "$w3_args"; then
    w3_browse_url "${baseUrl}"
else
    escaped_args=`w3_url_of_arg $w3_args`
    w3_browse_url "${searchUrl}''${escaped_args}"
fi
''
