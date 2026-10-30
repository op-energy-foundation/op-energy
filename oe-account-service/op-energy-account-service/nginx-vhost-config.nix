{config, ...}:
URL_BASE:
API_HOST:
let
  zones_enabled =
    if config.services ? "op-energy-account-service"
      then config.services.op-energy-account-service.enable
      else false;
in
{
  locations = {
    "${URL_BASE}api/v2/blockrate/ws" = {
      proxyPass = "${API_HOST}/api/v2/blockrate/ws";
      proxyWebsockets = true;
      extraConfig = if zones_enabled
        then ''
          limit_conn websocket 100;
        ''
        else "";
    };
    "${URL_BASE}api/v2/blockrate" = {
      proxyPass = "${API_HOST}/api/v2/blockrate";
      extraConfig = if zones_enabled
        then ''
          limit_req zone=api burst=10 nodelay;
        ''
        else "";
    };
    # service-to-service endpoints, which only other services may call over the
    # loopback interface: refused here, so no vhost built from this config can
    # expose them. "^~" takes precedence over the prefix match below and also
    # covers the path without a trailing slash
    "^~ ${URL_BASE}api/v2/account/internal" = {
      extraConfig = ''
        return 403;
      '';
    };
    "${URL_BASE}api/v2/account" = {
      proxyPass = "${API_HOST}/api/v2/account";
      extraConfig = if zones_enabled
        then ''
          limit_req zone=api burst=10 nodelay;
        ''
        else "";
    };
  };
}
