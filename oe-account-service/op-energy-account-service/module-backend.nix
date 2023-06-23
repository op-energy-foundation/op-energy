{ GIT_COMMIT_HASH}:
args@{config, pkgs, options, lib, ...}:
let
  op-energy-overlay = (import ./overlay.nix) { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
  initial_script = cfg:
    pkgs.writeText "initial_script.sql" ''
    do
    $$
    begin
      if not exists (select * from pg_user where usename = '${cfg.db_user}') then
        CREATE USER ${cfg.db_user} WITH PASSWORD '${cfg.db_psk}';
      end if;
    end
    $$
    ;
    ALTER USER ${cfg.db_user} WITH PASSWORD '${cfg.db_psk}';
  '';

  eachInstance = config.services.op-energy-backend;
  instanceOpts = args: {
    options = {
      db_name = lib.mkOption {
        default = null;
        type = lib.types.str;
        example = "mempool";
        description = "Database name of the instance";
      };
      account_db_name = lib.mkOption {
        default = null;
        type = lib.types.str;
        example = "mempoolacc";
        description = "Account database name of the instance";
      };
      db_user = lib.mkOption {
        default = null;
        type = lib.types.str;
        example = "mempool";
        description = "Username to access instance's database";
      };
      db_psk = lib.mkOption {
        type = lib.types.str;
        default = null;
        example = "your-secret-from-out-of-git-store";
        description = ''
          This value defines a password for database user, which will be used by op-energy backend instance to access database.
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
            "DB_NAME": "openergy",
            "DB_PASSWORD": "password",
            "SECRET_SALT": "salt",
            "API_HTTP_PORT": 8999,
            "BTC_URL": "http://127.0.0.1:8332",
            "BTC_USER": "op-energy",
            "BTC_PASSWORD": "password1",
            "BTC_POLL_RATE_SECS": 10,
            "SCHEDULER_POLL_RATE_SECS": 10
          }
        '';
      };
    };
  };
in
{
  options.services.op-energy-backend = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule instanceOpts);
    default = {};
    description = "One or more op-energy-backends";
    example = {
      mainnet = {
        config = ''
          {
            "DB_PORT": 5432,
            "DB_HOST": "127.0.0.1",
            "DB_USER": "openergy",
            "DB_NAME": "openergy",
            "DB_PASSWORD": "password",
            "SECRET_SALT": "salt",
            "API_HTTP_PORT": 8999,
            "BTC_URL": "http://127.0.0.1:8332",
            "BTC_USER": "op-energy",
            "BTC_PASSWORD": "password1",
            "BTC_POLL_RATE_SECS": 10,
            "SCHEDULER_POLL_RATE_SECS": 10
        '';
      };
    };
  };

  config = lib.mkIf (eachInstance != {}) {
    nixpkgs.overlays = [
      op-energy-overlay # add op-energy-backend into context
    ];
    environment.systemPackages = [ pkgs.op-energy-backend ];
    # enable postgresql and declare op-energy DB
    services.postgresql = {
      enable = true;
      ensureDatabases = (lib.mapAttrsToList (name: cfg:
        "${cfg.db_name}"
      ) eachInstance
      ) ++ (lib.mapAttrsToList (name: cfg:
        "${cfg.account_db_name}"
      ) eachInstance
      );
      ensureUsers = ( lib.mapAttrsToList (name: cfg:
        { name = "${cfg.db_user}";
          ensurePermissions = {
            "DATABASE ${cfg.db_name}" = "ALL PRIVILEGES";
          };
        }
      ) eachInstance
      ) ++ ( lib.mapAttrsToList (name: cfg:
        { name = "${cfg.db_user}";
          ensurePermissions = {
            "DATABASE ${cfg.account_db_name}" = "ALL PRIVILEGES";
          };
        }
      ) eachInstance
      );
    };
    systemd.services = {
      postgresql-op-energy-users = {
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
        script = lib.foldl' (acc: i: acc + i) '''' ( lib.mapAttrsToList (name: cfg: ''
          # create database if not exist. we can't use services.mysql.ensureDatabase/initialDatase here the latter
          # will not use schema and the former will only affects the very first start of mariadb service, which is not idemponent
          if [ ! "$(sudo -u postgres psql -l -x --csv | grep 'Name,${cfg.db_name}' --count)" == "1" ]; then
            ( echo 'CREATE DATABASE ${cfg.db_name};'
              echo '\c ${cfg.db_name};'
            ) | sudo -u postgres psql
          fi
          if [ ! "$(sudo -u postgres psql -l -x --csv | grep 'Name,${cfg.account_db_name}' --count)" == "1" ]; then
            ( echo 'CREATE DATABASE ${cfg.account_db_name};'
              echo '\c ${cfg.account_db_name};'
            ) | sudo -u postgres psql
          fi
          cat "${initial_script cfg}" | sudo -u postgres psql
        '') eachInstance);
      };
    } // ( lib.mapAttrs' (name: cfg: lib.nameValuePair "op-energy-backend-${name}" (
      let
        openergy_config = pkgs.writeText "op-energy-config.json" cfg.config; # this renders config and stores in /nix/store
      in {
        wantedBy = [ "multi-user.target" ];
        after = [
          "network-setup.service"
          "postgresql.service"
        ];
        requires = [
          "network-setup.service"
          "postgresql.service"
          ];
        serviceConfig = {
          Type = "simple";
          Restart = "always"; # we want to keep service always running, especially, now development instance is relying on ssh tunnel which can restart as well leading to op-energy restart as well
          StartLimitIntervalSec = 0;
          StartLimitBurst = 0;
        };
        path = with pkgs; [
          pkgs.op-energy-backend
        ];
        script = ''
          set -ex
          OPENERGY_BACKEND_CONFIG_FILE="${openergy_config}" op-energy-backend +RTS -c -N -s
        '';
      })) eachInstance);
  };
}
