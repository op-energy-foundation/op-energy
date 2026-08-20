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
