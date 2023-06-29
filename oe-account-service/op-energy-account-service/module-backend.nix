{ GIT_COMMIT_HASH}:
args@{config, pkgs, options, lib, ...}:
let
  op-energy-overlay = (import ../overlay.nix) { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
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

  cfg = config.services.op-energy-account-service;
in
{
  options.services.op-energy-account-service = {
    enable = lib.mkEnableOption "op-energy account service";
    api_port = lib.mkOption {
      type = lib.types.int;
      example = 8899;
      default = 8899;
      description = ''
        defines API port for an account service
      '';
    };
    metrics_port = lib.mkOption {
      type = lib.types.int;
      example = 7899;
      default = 7899;
      description = ''
        defines METRICS port for an account service
      '';
    };
    db_name = lib.mkOption {
      default = "openergyacc";
      type = lib.types.str;
      example = "openergyacc";
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
          "DB_NAME": "openergyacc",
          "DB_PASSWORD": "password",
          "SECRET_SALT": "salt",
          "ACCOUNT_TOKEN_ENCRYPTION_PRIVATE_KEY": "",
          "API_HTTP_PORT": 8899,
          "PROMETHEUS_PORT": 7899,
          "LOG_LEVEL_MIN": "Info",
          "SCHEDULER_POLL_RATE_SECS": 10
        }
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.nginx = {
      enable = true;
      appendConfig = ''
        worker_processes auto;
        worker_rlimit_nofile 100000;
      '';
      eventsConfig = ''
        worker_connections 9000;
        multi_accept on;
      '';
      serverTokens = false;
      clientMaxBodySize = "10m";
      commonHttpConfig = ''
        sendfile on;
        tcp_nopush on;
        tcp_nodelay on;

        server_name_in_redirect off;



        # reset timed out connections freeing ram
        reset_timedout_connection on;
        # maximum time between packets the client can pause when sending nginx any data
        client_body_timeout 10s;
        # maximum time the client has to send the entire header to nginx
        client_header_timeout 10s;
        # timeout which a single keep-alive client connection will stay open
        keepalive_timeout 69s;
        # maximum time between packets nginx is allowed to pause when sending the client data
        send_timeout 69s;

        # number of requests per connection, does not affect SPDY
        keepalive_requests 1337;

        # enable gzip compression
        gzip on;
        gzip_vary on;
        gzip_comp_level 6;
        gzip_min_length 1000;
        gzip_proxied expired no-cache no-store private auth;
        # text/html is always compressed by gzip module
        gzip_types application/javascript application/json application/ld+json application/manifest+json application/x-font-ttf application/x-web-app-manifest+json application/xhtml+xml application/xml font/opentype image/bmp image/svg+xml image/x-icon text/cache-manifest text/css text/plain text/vcard;

        # limit request body size

        # proxy cache
        proxy_cache off;
        proxy_cache_path /var/cache/nginx keys_zone=cache:20m levels=1:2 inactive=600s max_size=500m;

        # exempt localhost from rate limit
        geo $limited_ip {
                default         1;
                127.0.0.1       0;
        }
        map $limited_ip $limited_ip_key {
                1 $binary_remote_addr;
                0 \'\';
        }

        # rate limit requests
        limit_req_zone $limited_ip_key zone=api:5m rate=200r/m;
        limit_req_status 429;

        # rate limit connections
        limit_conn_zone $limited_ip_key zone=websocket:10m;
        limit_conn_status 429;

        server {
                listen 127.0.0.1:80;
        }
      '';
      virtualHosts.op-energy = {
        extraConfig = ''
          # enable browser and proxy caching
          add_header Cache-Control "public, no-transform";
          add_header 'Access-Control-Allow-Origin' '*' always;

          # vary cache if user changes language preference
          add_header Vary Accept-Language;
          add_header Vary Cookie;

          location /api/v1/account {
                  limit_req zone=api burst=10 nodelay;
                  proxy_pass http://127.0.0.1:${toString cfg.api_port}/api/v1/account;
          }
        '';
      };
    };

    nixpkgs.overlays = [
      op-energy-overlay # add op-energy-backend into context
    ];
    environment.systemPackages = [ pkgs.op-energy-api pkgs.op-energy-account-service ];
    # enable postgresql and declare op-energy DB
    services.postgresql = {
      enable = true;
      ensureDatabases = [ "${cfg.db_name}" ];
      ensureUsers = [ {
        name = "${cfg.db_user}";
        ensurePermissions = {
          "DATABASE ${cfg.db_name}" = "ALL PRIVILEGES";
        };
      } ];
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
        script = ''
          # create database if not exist. we can't use services.mysql.ensureDatabase/initialDatase here the latter
          # will not use schema and the former will only affects the very first start of mariadb service, which is not idemponent
          if [ ! "$(sudo -u postgres psql -l -x --csv | grep 'Name,${cfg.db_name}' --count)" == "1" ]; then
            ( echo 'CREATE DATABASE ${cfg.db_name};'
              echo '\c ${cfg.db_name};'
            ) | sudo -u postgres psql
          fi
          cat "${initial_script cfg}" | sudo -u postgres psql
        '';
      };
      op-energy-account-service =
      let
        openergy_config = pkgs.writeText "op-energy-account-service-config.json" cfg.config; # this renders config and stores in /nix/store
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
          pkgs.op-energy-account-service
        ];
        script = ''
          set -ex
          OPENERGY_BACKEND_CONFIG_FILE="${openergy_config}" op-energy-account-service +RTS -c -N -s
        '';
      };
    };
  };
}
