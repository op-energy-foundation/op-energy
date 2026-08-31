{config, ...}:
URL_BASE:
API_HOST:
let
  zones_enabled =
    if config.services ? "op-energy-offer-service"
      then config.services.op-energy-offer-service.enable
      else false;
in
{
  locations = {
    "${URL_BASE}api/v2/offer" = {
      proxyPass = "${API_HOST}/api/v2/offer";
      extraConfig = if zones_enabled
        then ''
          limit_req zone=api burst=10 nodelay;
        ''
        else "";
    };
  };
}
