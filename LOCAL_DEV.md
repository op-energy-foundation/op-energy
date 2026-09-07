# Local Development Guide

Run the full op-energy stack locally using Docker for backend services and Vite
for the frontend.

## Prerequisites

- Docker Desktop running
- Node.js 18+ (for frontend)
- The following repos cloned side by side:
  ```
  op-energy-repos/
    op-energy-foundation/          # this repo (account + offer services)
    op-energy-blockspan-service/   # sibling repo (required by nix build)
    op-energy-mvp-dev/             # MVP frontend
  ```

## 1. Start PostgreSQL

A postgres container named `postgres-openergy` should already exist:

```bash
docker ps | grep postgres-openergy
```

If not, create one:

```bash
docker run -d --name postgres-openergy \
  -e POSTGRES_USER=openergy \
  -e POSTGRES_PASSWORD=OpEnergy2024 \
  -e POSTGRES_DB=openergy \
  -p 5432:5432 \
  postgres:14
```

Create the databases:

```bash
docker exec postgres-openergy psql -U openergy -c "CREATE DATABASE openergyacc;"
docker exec postgres-openergy psql -U openergy -c "CREATE DATABASE openergyoffer;"
```

## 2. Start Nix Build Container

```bash
docker run -d --name op-energy-nix \
  -v "$(pwd)/..:/repos" \
  -p 8899:8899 -p 8909:8909 \
  --add-host=host.docker.internal:host-gateway \
  nixos/nix:latest tail -f /dev/null
```

**Important:** Use `-p 8899:8899 -p 8909:8909` for port mappings so APIs are
accessible from the host. Use `--add-host=host.docker.internal:host-gateway` so
the container can reach postgres on the host.

## 3. Patch Main.hs (disable blockspan sync)

The account service's `Main.hs` starts blockspan websocket/scheduler threads
that crash-loop without a blockspan service, killing the HTTP server via
`waitAnyCancel`. Comment them out for local dev:

```bash
# In oe-account-service/op-energy-account-service/app/Main.hs
# Comment out these thread spawns:
#   schedulerBlockTimeStrikeA
#   blockTimeStrikeWebsocketClientA
#   blockTimeStrikeNewTipHandlerA
# And remove them from the waitAnyCancel list.
# Keep only: serverA, schedulerA, prometheusA
```

The diff looks like:

```diff
-  schedulerBlockTimeStrikeA <- liftIO $ asyncBound $ runAppT state $ do
-    runLogging $ $(logInfo) "scheduler thread blocktime strike"
-    BlockTimeStrike.schedulerMainLoop
-  blockTimeStrikeWebsocketClientA <- liftIO $ asyncBound $ runAppT state $ do
-    runLogging $ $(logInfo) "block time strike websocket client API"
-    BlockTimeStrike.runBlockSpanClient
   serverA <- liftIO $ asyncBound $ runAppT state $ do
     runLogging $ $(logInfo) "serving API"
     runServer
-  blockTimeStrikeNewTipHandlerA <- liftIO $ asyncBound $ runAppT state $ do
-    runLogging $ $(logInfo) "starting newTipHandler"
-    BlockTimeStrike.newTipHandlerLoop
   liftIO $ waitAnyCancel $
     [ serverA
     , schedulerA
     , prometheusA
-    , schedulerBlockTimeStrikeA
-    , blockTimeStrikeWebsocketClientA
-    , blockTimeStrikeNewTipHandlerA
     ]
```

**Do NOT commit this change.** Revert before pushing:
```bash
git checkout -- oe-account-service/op-energy-account-service/app/Main.hs
```

## 4. Build Services

```bash
# Build account service (first build takes ~20 min, subsequent builds ~2 min)
ACCT=$(docker exec op-energy-nix bash -c 'cd /repos/op-energy-foundation && \
  nix-build -A op-energy-account-service "overlay-set.nix" \
  --arg GIT_COMMIT_HASH \"test\" --no-out-link 2>/dev/null')
echo "Account: $ACCT"

# Build offer service
OFFER=$(docker exec op-energy-nix bash -c 'cd /repos/op-energy-foundation && \
  nix-build -A op-energy-offer-service "overlay-set.nix" \
  --arg GIT_COMMIT_HASH \"test\" --no-out-link 2>/dev/null')
echo "Offer: $OFFER"
```

## 5. Create Config Files

```bash
ENCRYPTION_KEY=$(dd if=/dev/urandom bs=1 count=96 2>/dev/null | base64 | tr -d '\n')
SHARED_SECRET=$(dd if=/dev/urandom bs=1 count=32 2>/dev/null | base64 | tr -d '\n')

python3 -c "
import json

acct = {
    'DB_PORT': 5432, 'DB_HOST': 'host.docker.internal',
    'DB_USER': 'openergy', 'DB_NAME': 'openergyacc',
    'DB_PASSWORD': 'OpEnergy2024',
    'SECRET_SALT': 'localtest',
    'ACCOUNT_TOKEN_ENCRYPTION_PRIVATE_KEY': '$ENCRYPTION_KEY',
    'API_HTTP_PORT': 8899, 'PROMETHEUS_PORT': 7899,
    'LOG_LEVEL_MIN': 'Info', 'SCHEDULER_POLL_RATE_SECS': 600,
    'WEBSOCKET_KEEP_ALIVE_SECS': 30,
    'BLOCKTIME_STRIKE_BLOCKSPAN_WEBSOCKET_API_URL': 'http://127.0.0.1:8999/api/v1/ws',
    'BLOCKSPAN_API_URL': 'http://127.0.0.1:8999',
    'INTERNAL_SERVICE_SHARED_SECRET': '$SHARED_SECRET'
}
with open('/tmp/account-config.json', 'w') as f: json.dump(acct, f)

offer = {
    'DB_PORT': 5432, 'DB_HOST': 'host.docker.internal',
    'DB_USER': 'openergy', 'DB_NAME': 'openergyoffer',
    'DB_PASSWORD': 'OpEnergy2024',
    'API_HTTP_PORT': 8909, 'PROMETHEUS_PORT': 7909,
    'LOG_LEVEL_MIN': 'Info', 'SCHEDULER_POLL_RATE_SECS': 600,
    'ACCOUNT_SERVICE_API_URL': 'http://127.0.0.1:8899',
    'INTERNAL_SERVICE_SHARED_SECRET': '$SHARED_SECRET'
}
with open('/tmp/offer-config.json', 'w') as f: json.dump(offer, f)
print('configs written')
"

docker cp /tmp/account-config.json op-energy-nix:/tmp/account-config.json
docker cp /tmp/offer-config.json op-energy-nix:/tmp/offer-config.json
```

## 6. Start Services

```bash
# Start account service
docker exec -d op-energy-nix bash -c \
  "OPENERGY_ACCOUNT_SERVICE_CONFIG_FILE=/tmp/account-config.json \
   $ACCT/bin/op-energy-account-service +RTS -N > /tmp/acct.log 2>&1"

sleep 5

# Verify account service is up
docker exec op-energy-nix bash -c \
  'timeout 2 bash -c "echo > /dev/tcp/127.0.0.1/8899" 2>&1 && echo "ACCOUNT UP" || echo "NOT UP"'

# Start offer service
docker exec -d op-energy-nix bash -c \
  "OPENERGY_OFFER_SERVICE_CONFIG_FILE=/tmp/offer-config.json \
   $OFFER/bin/op-energy-offer-service +RTS -N > /tmp/offer.log 2>&1"

sleep 5

# Verify offer service is up
docker exec op-energy-nix bash -c \
  'timeout 2 bash -c "echo > /dev/tcp/127.0.0.1/8909" 2>&1 && echo "OFFER UP" || echo "NOT UP"'
```

## 7. Test Backend APIs

```bash
# Register
curl -s -X POST http://localhost:8899/api/v1/account/register

# Whoami (use token from register response)
curl -s http://localhost:8899/api/v2/account/whoami -H "Authorization: <TOKEN>"

# Offer list
curl -s http://localhost:8909/api/v2/offer/list

# Post offer
curl -s -X POST http://localhost:8909/api/v2/offer/post \
  -H "Authorization: <TOKEN>" \
  -H "Content-Type: application/json" \
  -d '{"targetBlock":900000,"validTillBlock":900100,"numberOfOffers":1,"makerStakeSats":50000}'
```

## 8. Start Frontend

```bash
cd ../op-energy-mvp-dev
npm install
npm run dev
```

The Vite dev server starts on `http://localhost:8080` and proxies API calls to
the local backend (see `vite.config.ts` proxy config — may need updating to
point to `localhost:8899` / `localhost:8909`).

## Cleanup

```bash
# Stop services
docker exec op-energy-nix bash -c 'pkill -f op-energy-account; pkill -f op-energy-offer'

# Revert Main.hs patch
git checkout -- oe-account-service/op-energy-account-service/app/Main.hs

# Stop container (keeps nix cache for fast rebuilds)
docker stop op-energy-nix

# Or remove entirely (loses nix cache — next build takes 20+ min)
docker rm -f op-energy-nix
```

## Troubleshooting

| Problem | Fix |
|---------|-----|
| Account service crashes immediately | Blockspan threads not commented out. See step 3. |
| `Connection refused` on port 8899/8909 | Services not started, or container lacks port mappings. Check `docker ps`. |
| `balance column contains null values` | Run the DB migration manually or ensure the custom migration in `DB.hs` runs before `runMigration`. |
| `op-energy-nix` container missing after Docker restart | Recreate with step 2. The nix store is lost — rebuild takes ~20 min. |
| Build takes forever | First build compiles all Haskell deps (~20 min). Subsequent builds reuse the nix store (~2 min). Don't `docker rm` the container. |
