# Deployment Guide

How to deploy op-energy services to dev/prod instances.

## Architecture

```
op-energy (this repo)              — backend Haskell code
op-energy-dev-instance             — NixOS server config (host.nix, local_settings_*.nix)
op-energy-blockspan-service        — sibling repo, blockspan service
```

The dev instance config repo (`op-energy-dev-instance`) has:
- `host.nix` — **shared** between dev and prod, imports all service modules
- `local_settings.nix` — routes to per-host config based on hostname
- `local_settings_op-energy-dev.nix` — dev server (`dev-exchange.op-energy.info`)
- `local_settings_op-energy.nix` — prod server (`exchange.op-energy.info`)

## Server Access

```bash
# Dev server
SSH_AUTH_SOCK="" ssh -o IdentitiesOnly=yes -i ~/.ssh/naimish_ed25519 root@dev-exchange.op.energy

# The SSH_AUTH_SOCK="" and -o IdentitiesOnly=yes are required because
# having too many keys in the agent causes "Too many authentication failures"
```

## Auto-Deploy

The `devel-auto-deploy.yml` workflow triggers **only** on pushes to the `devel`
branch of this repo (`op-energy-foundation/op-energy`). It:

1. SSHs to the dev server
2. Runs `cd /etc/nixos; git pull --rebase; git submodule update --init --remote --recursive`
3. Runs `nixos-rebuild switch`

**Important:** Changes to the `op-energy-dev-instance` repo do NOT auto-deploy.
You must manually SSH to the server, pull, and rebuild.

## Adding a New Service

When adding a new microservice (example: oe-offer-service):

### 1. op-energy repo (this repo)

- Create `module-backend.nix` with:
  - NixOS service options (`enable`, `api_port`, `db_name`, etc.)
  - systemd service unit
  - postgresql database setup
  - nginx proxy for the `.op.energy` domain (the main virtualhost)
- Create `nginx-vhost-config.nix` — reusable function for adding the proxy to
  other virtualhosts (like `.info` domain)
- Update root `overlay-set.nix` — add the new service overlay
- Add DB migration if adding columns to existing tables (see below)

### 2. op-energy-dev-instance repo

- `host.nix`:
  - Import the new module: `opEnergyOfferServiceModule = import ./overlays/op-energy/oe-offer-service/...`
  - Add to `imports` list
  - Add `services.op-energy-<name>` config block with DB credentials and ports
  - Add any new secrets (read from `/etc/nixos/private/`)
  - Add `INTERNAL_SERVICE_SHARED_SECRET` or similar to any existing service
    that the new one calls

- `local_settings_op-energy-dev.nix` (dev):
  - Add nginx proxy to the `.info` domain virtualhost using `nginx-vhost-config`:
    ```nix
    (pkgs.op-energy-offer-service-nginx-vhost-config { config = config; } "/" "http://127.0.0.1:8909")
    ```

- `local_settings_op-energy.nix` (prod):
  - Same as dev but for the prod domain

### 3. On the server

```bash
# Create any secret files
dd if=/dev/urandom bs=1 count=32 2>/dev/null | base64 -w 0 > /etc/nixos/private/op-energy-internal-service-shared-secret.txt

# Pull and rebuild
cd /etc/nixos
git pull --rebase
git submodule update --init --remote --recursive
nixos-rebuild switch
```

## Common Pitfalls

### 1. DB migrations for new NOT NULL columns

Persistent's `runMigration` tries `ALTER TABLE ADD COLUMN ... NOT NULL` which
**fails** if existing rows exist (they'd have NULL values).

**Fix:** Add a custom migration in `accountDBMigrations` (in
`oe-account-service/.../V1/DB.hs`) that runs BEFORE `runMigration`. The
migration should check if the column exists and add it with a `DEFAULT`:

```haskell
accountDBMigrations =
  [ (\_-> return ())
  , (\_-> do -- add balance column with default for existing rows
      (mColumnExists::[Single Bool]) <- rawSql
        "SELECT EXISTS (SELECT FROM information_schema.columns \
        \WHERE table_name = 'person' AND column_name = 'balance' \
        \AND table_schema = 'public');"
        []
      case mColumnExists of
        ((Single True):_) -> return ()
        _ -> rawExecute
          "ALTER TABLE person ADD COLUMN balance INT8 NOT NULL DEFAULT 300000;"
          []
    )
  ]
```

**This list is append-only.** Never delete or modify existing entries.

### 2. nginx duplicate `limit_req`

The `nginx-vhost-config.nix` functions add `limit_req zone=api` to each
location. The `module-backend.nix` also adds `limit_req` via `extraConfig` for
the same locations on the main virtualhost.

**Rule:** Do NOT use both `module-backend.nix` extraConfig AND
`nginx-vhost-config` functions for the **same virtualhost**. They're designed
for different purposes:
- `module-backend.nix` → adds routes to the `op-energy` virtualhost (`.op.energy` domain)
- `nginx-vhost-config.nix` → adds routes to OTHER virtualhosts (like `.info` domain)

If nginx refuses to start with `"limit_req" directive is duplicate`, check for
double-declaration of the same location.

### 3. Account service crash-loops on startup

The blockspan websocket thread (`BlockTimeStrike.runBlockSpanClient`) connects
to port 8999. If the blockspan service isn't ready, it throws
`Network.Socket.connect: Connection refused` and `waitAnyCancel` kills all
threads — including the HTTP server.

**Fix:** Just restart after the blockspan service is up:
```bash
systemctl restart op-energy-account-service
```

### 4. Nix evaluation caching

`nixos-rebuild switch` caches derivation hashes. Changing a file in a submodule
doesn't always invalidate the cache.

**Fix:** Ensure the submodule ref has actually changed:
```bash
cd /etc/nixos
git submodule update --init --remote --recursive
nixos-rebuild switch
```

### 5. Services on different domains

The dev instance serves two domains:
- `dev-exchange.op.energy` — main domain, gets API routes from `module-backend.nix`
- `dev-exchange.op-energy.info` — MVP frontend domain, needs explicit proxy config

API routes on `.op.energy` are added automatically by each service's
`module-backend.nix`. Routes on `.info` must be added manually in
`local_settings_op-energy-dev.nix` using the `nginx-vhost-config` pattern.

## Secret Files

All secrets live in `/etc/nixos/private/` on each server. They are **never**
stored in git. Create them once per server:

| File | Used by | Generate with |
|------|---------|---------------|
| `bitcoind-mainnet-rpc-psk.txt` | blockspan service | manual |
| `op-energy-db-psk-mainnet.txt` | all services | manual |
| `op-energy-db-salt-mainnet.txt` | account service | manual |
| `op-energy-account-token-encryption-key.txt` | account service | `dd if=/dev/urandom bs=1 count=96 2>/dev/null \| base64 -w 0` |
| `op-energy-internal-service-shared-secret.txt` | account + offer service | `dd if=/dev/urandom bs=1 count=32 2>/dev/null \| base64 -w 0` |

The shared secret must be **identical** in both the account service and offer
service configs (they use it to authenticate cross-service calls).
