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
        CREATE USER ${cfg.db_user} WITH PASSWORD 'DB_PASSWORD_SECRET';
      end if;
      ALTER USER ${cfg.db_user} WITH PASSWORD 'DB_PASSWORD_SECRET';
      GRANT ALL PRIVILEGES ON DATABASE ${cfg.db_name} TO ${cfg.db_user};
      ALTER DATABASE ${cfg.db_name} OWNER TO ${cfg.db_user};
    end
    $$
    ;
  '';
  inject_credentials = cfg: file: pkgs.writeScriptBin "inject_credentials" ''
    cat >> ${file} <<EOF
      "DB_PASSWORD": "$(cat $CREDENTIALS_DIRECTORY/DB_PASSWORD_SECRET)",
      "INTERNAL_SERVICE_SHARED_SECRET": "$(cat $CREDENTIALS_DIRECTORY/INTERNAL_SERVICE_SHARED_SECRET_SECRET)"
    }
    EOF
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
      example = "openergyoffer";
      description = "Username to access instance's database";
    };
    credentials_locations = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {
        DB_PASSWORD_SECRET = "/etc/nixos/private/OP_ENERGY_OFFER_DB_PASSWORD_SECRET";
        INTERNAL_SERVICE_SHARED_SECRET_SECRET = "/etc/nixos/private/INTERNAL_SERVICE_SHARED_SECRET";
      };
      description = ''
        A set of credentials (by it's name) and file path containing it.
        File path is expected to be only readable by the root user.
        In the usage example, DB_PASSWORD_SECRET will be replaced within config located at
        $OPENERGY_OFFER_SERVICE_CONFIG_FILE with a content of the /etc/nixos/private/DB_PASSWORD_SECRET
        '';
      example = {
        DB_PASSWORD_SECRET = "/etc/nixos/private/OP_ENERGY_OFFER_DB_PASSWORD_SECRET";
        INTERNAL_SERVICE_SHARED_SECRET_SECRET = "/etc/nixos/private/INTERNAL_SERVICE_SHARED_SECRET";
      };
    };
    config = lib.mkOption {
      type = lib.types.str;
      default = "";
      example = ''
          "DB_PORT": 5432,
          "DB_HOST": "127.0.0.1",
          "API_HTTP_PORT": 8909,
          "PROMETHEUS_PORT": 7909,
          "LOG_LEVEL_MIN": "Info",
          "SCHEDULER_POLL_RATE_SECS": 10,
          "ACCOUNT_SERVICE_API_URL": "http://127.0.0.1:8899",
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
    users.users.op-energy-offer =
      {
        isNormalUser = true;
        group = "op-energy-offer";
        createHome = true;
      };
    users.groups.op-energy-offer =
      {
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
          LoadCredential =
            [ "DB_PASSWORD_SECRET:${cfg.credentials_locations.DB_PASSWORD_SECRET}"
            ];
          User = "postgres";
          Group = "postgres";
        };
        path = with pkgs; [
          gnused postgresql
        ];
        preStart = let
          iteration = pkgs.writeScriptBin "iteration" ''
          # create database if not exist. we can't use services.mysql.ensureDatabase/initialDatase here the latter
          # will not use schema and the former will only affects the very first start of mariadb service, which is not idemponent
          if [ ! "$(psql -l -x --csv | grep 'Name,${cfg.db_name}' --count)" == "1" ]; then
            ( echo 'CREATE DATABASE ${cfg.db_name};'
              echo '\c ${cfg.db_name};'
            ) | psql
          fi
          cat "${initial_script cfg}" \
            | sed "s|DB_PASSWORD_SECRET|$(cat $CREDENTIALS_DIRECTORY/DB_PASSWORD_SECRET)|g" \
            | psql 2>/dev/null
          for tbl in `psql -qAt -c "select tablename from pg_tables where schemaname = 'public';" ${cfg.db_name}` ; do
            psql -c "alter table \"$tbl\" owner to ${cfg.db_user}" ${cfg.db_name}
          done
          for tbl in `psql -qAt -c "select sequence_name from information_schema.sequences where sequence_schema = 'public';" ${cfg.db_name}` ; do
            psql -c "alter sequence \"$tbl\" owner to ${cfg.db_user}" ${cfg.db_name}
          done
          for tbl in `psql -qAt -c "select table_name from information_schema.views where table_schema = 'public';" ${cfg.db_name}` ; do
            psql -c "alter view \"$tbl\" owner to ${cfg.db_user}" ${cfg.db_name}
          done
        '';
        in ''
          COUNT=0
          MAX_COUNT=10
          while [ "$COUNT" -lt "$MAX_COUNT" ]; do
            ${iteration}/bin/iteration && exit 0 || {
              sleep 1s
              COUNT=$(( $COUNT + 1 ))
            }
          done
          echo "was not able to update DB user and passwords"
          exit 1
        '';
        script = "exit 0";
      };
      op-energy-offer-service =
      let
        openergy_config = pkgs.writeText "op-energy-offer-service-config.json" ''
        {
          "DB_USER": "${cfg.db_user}",
          "DB_NAME": "${cfg.db_name}",
          ${cfg.config}
        '';
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
          "postgresql-op-energy-offer-users.service"
          ];
        serviceConfig = {
          Type = "simple";
          Restart = "always";
          RestartSec = "10s";
          LoadCredential =
            [ "DB_PASSWORD_SECRET:${cfg.credentials_locations.DB_PASSWORD_SECRET}"
              "INTERNAL_SERVICE_SHARED_SECRET_SECRET:${cfg.credentials_locations.INTERNAL_SERVICE_SHARED_SECRET_SECRET}"
            ];
          User =  "op-energy-offer";
          Group = "op-energy-offer";
        };
        path = with pkgs; [
          pkgs.op-energy-offer-service
        ];
        script = ''
          set -ex
          mkdir -p ~/.op-energy-offer || true
          rm -f ~/.op-energy-offer/config.json || true
          cp ${openergy_config} ~/.op-energy-offer/config.json
          chmod u+w ~/.op-energy-offer/config.json
          chmod og-rwx ~/.op-energy-offer/config.json
          ${inject_credentials cfg "~/.op-energy-offer/config.json"}/bin/inject_credentials
          OPENERGY_OFFER_SERVICE_CONFIG_FILE=~/.op-energy-offer/config.json \
            op-energy-offer-service +RTS -c -N -s
        '';
      };
    };
  };
}
