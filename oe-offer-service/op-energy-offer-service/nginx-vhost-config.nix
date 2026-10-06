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
    "${URL_BASE}api/v1/offer/ws" = {
      proxyPass = "${API_HOST}/api/v1/offer/ws";
      proxyWebsockets = true;
      extraConfig = if zones_enabled
        then ''
          limit_conn websocket 100;
        ''
        else "";
    };
    "${URL_BASE}api/v1/offer" = {
      proxyPass = "${API_HOST}/api/v1/offer";
      extraConfig = if zones_enabled
        then ''
          limit_req zone=api burst=10 nodelay;
        ''
        else "";
    };
  };
}
