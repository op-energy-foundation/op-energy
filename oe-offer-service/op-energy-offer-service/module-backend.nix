{ GIT_COMMIT_HASH}:
args@{config, pkgs, options, lib, ...}:
let
  op-energy-overlay = (import ../../overlay.nix) { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
  initial_script = cfg:
    pkgs.writeText "initial_script.sql" ''
    do
    $$
    begin
      if not exists (select * from pg_user where usename = '${cfg.db_user}') then
        CREATE USER ${cfg.db_user} WITH PASSWORD '${cfg.db_psk}';
      end if;
      ALTER USER ${cfg.db_user} WITH PASSWORD '${cfg.db_psk}';
      GRANT ALL PRIVILEGES ON DATABASE ${cfg.db_name} TO ${cfg.db_user};
      ALTER DATABASE ${cfg.db_name} OWNER TO ${cfg.db_user};
    end
    $$
    ;
  '';

  cfg = config.services.op-energy-offer-service;
in
{
  options.services.op-energy-offer-service = {
    enable = lib.mkEnableOption "op-energy offer service";
    api_port = lib.mkOption {
      type = lib.types.int;
      example = 8909;
      default = 8909;
      description = ''
        defines API port for the offer service
      '';
    };
    metrics_port = lib.mkOption {
      type = lib.types.int;
      example = 7909;
      default = 7909;
      description = ''
        defines METRICS port for the offer service
      '';
    };
    db_name = lib.mkOption {
      default = "openergyoffer";
      type = lib.types.str;
      example = "openergyoffer";
      description = "Database name of the instance";
    };
    db_user = lib.mkOption {
      default = null;
      type = lib.types.str;
      example = "openergy";
      description = "Username to access instance's database";
    };
    db_psk = lib.mkOption {
      type = lib.types.str;
      default = null;
      example = "your-secret-from-out-of-git-store";
      description = ''
        This value defines a password for database user, which will be used by op-energy offer service instance to access database.
      '';
    };
    config = lib.mkOption {
      type = lib.types.str;
      default = "";
      example = ''
        {
          "DB_PORT": 5432,
          "DB_HOST": "127.0.0.1",
          "DB_USER": "openergy",
          "DB_NAME": "openergyoffer",
          "DB_PASSWORD": "password",
          "API_HTTP_PORT": 8909,
          "PROMETHEUS_PORT": 7909,
          "LOG_LEVEL_MIN": "Info",
          "SCHEDULER_POLL_RATE_SECS": 10,
          "ACCOUNT_SERVICE_API_URL": "http://127.0.0.1:8899",
          "INTERNAL_SERVICE_SHARED_SECRET": "your-secret-from-out-of-git-store -- must match oe-account-service's own value"
        }
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [
      op-energy-overlay
    ];
    environment.systemPackages = [ pkgs.op-energy-api pkgs.op-energy-offer-service ];
    services.postgresql = {
      enable = true;
      ensureDatabases = [ "${cfg.db_name}" ];
      ensureUsers =
        [ { name = "${cfg.db_user}"; }
        ];
    };
    systemd.services = {
      postgresql-op-energy-offer-users = {
        wantedBy = [ "multi-user.target" ];
        after = [
          "postgresql.service"
        ];
        requires = [
          "postgresql.service"
        ];
        serviceConfig = {
          Type = "simple";
        };
        path = with pkgs; [
          postgresql sudo
        ];
        preStart = ''
          if [ ! "$(sudo -u postgres psql -l -x --csv | grep 'Name,${cfg.db_name}' --count)" == "1" ]; then
            ( echo 'CREATE DATABASE ${cfg.db_name};'
              echo '\c ${cfg.db_name};'
            ) | sudo -u postgres psql || true
          fi
          cat "${initial_script cfg}" | sudo -u postgres psql || true
        '';
        script = "exit 0";
      };
      op-energy-offer-service =
      let
        openergy_config = pkgs.writeText "op-energy-offer-service-config.json" cfg.config;
      in {
        wantedBy = [ "multi-user.target" ];
        after = [
          "network-online.target"
          "postgresql.service"
          "postgresql-op-energy-offer-users.service"
        ];
        requires = [
          "postgresql.service"
          "network-online.target"
          ];
        serviceConfig = {
          Type = "simple";
          Restart = "always";
          StartLimitIntervalSec = 0;
          StartLimitBurst = 0;
        };
        path = with pkgs; [
          pkgs.op-energy-offer-service
        ];
        script = ''
          set -ex
          OPENERGY_OFFER_SERVICE_CONFIG_FILE="${openergy_config}" op-energy-offer-service +RTS -c -N -s
        '';
      };
    };
    networking.firewall = {
      allowedTCPPorts = [
        cfg.api_port
      ];
    };
  };
}
