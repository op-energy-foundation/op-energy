# Sprint 3: Settlement Engine — Design Spec

## Goal

Complete the core trading loop: users accept offers, offers become trades,
trades confirm at the target block, and settle after 6 confirmations with
automatic fund distribution.

## Status Lifecycle

```
                ┌─────────┐
                │  OPEN   │
                └────┬────┘
                     │
        ┌────────────┼────────────┐
        │            │            │
        ▼            ▼            ▼
  ┌──────────┐ ┌──────────┐ ┌─────────┐
  │ ACCEPTED │ │ CANCELLED│ │ EXPIRED │
  └────┬─────┘ └──────────┘ └─────────┘
       │
       ▼
  ┌───────────┐
  │CONFIRMING │
  └────┬──────┘
       │
       ▼
  ┌──────────┐
  │ SETTLED  │
  └──────────┘
```

| From       | To         | Trigger                                     | Funds                                         |
|------------|------------|---------------------------------------------|-----------------------------------------------|
| Open       | Accepted   | Taker calls `POST /accept`                  | Taker's `makerStakeSats` deducted             |
| Open       | Cancelled  | Maker calls `POST /cancel`                  | Maker's stake refunded (existing)             |
| Open       | Expired    | Scheduler: `validTillBlock <= confirmedTip`  | Maker's stake refunded (existing)             |
| Accepted   | Confirming | Scheduler: `targetBlock <= confirmedTip`     | No funds move                                 |
| Confirming | Settled    | Scheduler: `targetBlock + 6 <= confirmedTip` | Winner credited `(pot - fee)`, fee burned     |

**No cancellation after accept.** Once accepted, the offer is a binding contract.

## Offer Table Extensions

New columns added to the existing `Offer` entity (all `Maybe` for migration
compatibility with existing rows):

| Field              | Type              | Populated when   |
|--------------------|-------------------|------------------|
| `blockRate`        | `Double`          | On create        |
| `predictedBlockMTP`| `UTCTime`         | On create        |
| `takerUUID`        | `UUID Person`     | On accept        |
| `takerDisplayName` | `DisplayName`     | On accept        |
| `winnerUUID`       | `UUID Person`     | On settlement    |
| `payoutSats`       | `Word64`          | On settlement    |
| `acceptedAt`       | `UTCTime`         | On accept        |
| `confirmedAt`      | `UTCTime`         | On confirm       |
| `settledAt`        | `UTCTime`         | On settlement    |

DB migration appended to `offerDBMigrations` — each column added with
`ALTER TABLE offer ADD COLUMN ... DEFAULT NULL`.

## New Table: PlatformStats

Single-row table in the offer DB to track accumulated fees:

```
PlatformStats
  totalFeesCollectedSats  Word64
  lastUpdated             UTCTime
```

Incremented on each settlement by `platformFeeSats`.

## Validation Rules

| Field           | Rule                                                |
|-----------------|-----------------------------------------------------|
| `blockRate`     | `>= 0.30 AND <= 20.0`                              |
| `makerStakeSats`| `> platformFeeSats` (default fee: 1,000 sats)       |
| `targetBlock`   | `> currentTipHeight` (must be in the future)        |
| `platformFeeSats`| Configurable per deployment, default 1,000 sats    |

## Predicted Block MTP Computation

**Computed server-side** to prevent client-side manipulation. Frontend sends
only `blockRate` (the user's selected min/block) and `targetBlock`.

Backend computes:

```
blocksAhead     = targetBlock - unconfirmedTipHeight
estMinutes      = blocksAhead * blockRate
predictedBlockMTP = unconfirmedTipMTP + estMinutes (in minutes)
```

Uses the **unconfirmed tip** for predictions (matches what users see on
mempool.space). Uses the **confirmed tip** for settlement (safe from reorgs).

## Settlement Logic

**Winner determination:** When `targetBlock + 6 <= confirmedTip`:

1. Fetch actual block MTP for `targetBlock` from blockspan REST:
   `GET /api/v1/oe/blockbyheight/:targetBlock`
2. Compare `actualBlockMTP` vs `predictedBlockMTP`:
   - If `actualBlockMTP < predictedBlockMTP` → **maker wins**
   - If `actualBlockMTP >= predictedBlockMTP` → **taker wins**
3. Credit winner: `(makerStakeSats * 2) - platformFeeSats`
4. Increment `PlatformStats.totalFeesCollectedSats += platformFeeSats`
5. Set `winnerUUID`, `payoutSats`, `settledAt`, `status = settled`

**Fee sats are burned** (not credited to any account). Tracked in
`PlatformStats` for revenue visibility. A platform wallet can be added
in a future sprint.

**Settlement failure:** If `creditBalance` to the winner fails, the offer
stays as `confirming`. The scheduler retries next tick. Settlement is
idempotent — same block MTP produces the same winner every time.

## Concurrent Accept Race Condition

Two takers call `POST /accept` on the same offer simultaneously:

1. Both verify token via `whoami`
2. Both attempt `deductBalance` from their account
3. Both attempt atomic DB flip: `UPDATE offer SET status='accepted' WHERE id=? AND status='open'`
4. `updateWhereCount` returns 0 for the loser (someone else already flipped)
5. Loser is **immediately refunded** via `creditBalance`
6. Loser gets an error response: "offer already accepted"

No pre-check of balance — just attempt the deduct and handle the
`InsufficientBalance` error from the account service.

## New API Endpoints

### `POST /api/v2/offer/:id/accept`

**Auth:** Required (Authorization header with account token)

**Flow:**
1. Verify taker's token via `AccountClient.verifyAccountToken`
2. Load offer by ID, check `status == open`
3. Check `taker != maker` (can't accept your own offer)
4. Check `validTillBlock > confirmedTip` (not expired)
5. Deduct `makerStakeSats` from taker via `AccountClient.deductBalance`
6. Atomic flip: `status = open → accepted`, set `takerUUID`,
   `takerDisplayName`, `acceptedAt`
7. If flip fails (race), refund taker immediately
8. Return updated `OfferInfo`

**Errors:**
- 401: invalid token
- 404: offer not found
- 400: offer not open / can't accept own offer / insufficient balance
- 409: offer already accepted (race condition)
- 502: account service unavailable

### `GET /api/v2/offer/trades`

**Auth:** Required

**Returns:** Paginated list of offers where the user is maker OR taker,
with status in `[accepted, confirming, settled]`. Newest first.

**Query params:** `page`, `limit` (same pagination as existing `list`).

## Block Height Source

New background thread: **blockspan WebSocket client** with graceful reconnect.

### Connection
- Connects to blockspan at `BLOCKSPAN_WS_URL` (default: `ws://127.0.0.1:8999/api/v1/ws`)
- Sends `{"action": "init"}` on connect to get current state
- Listens for `MessageNewestBlockHeader` messages

### State
Stores **two values** in the `State` record:
- `confirmedTip :: TVar (Maybe BlockHeight)` — from `oe-newest-confirmed-block`
- `unconfirmedTip :: TVar (Maybe BlockHeight)` — from `oe-latest-unconfirmed-block-height`

### Reconnect
On disconnect: log warning, wait 5 seconds, reconnect. Never crash.
If blockspan is down at startup, both TVars stay `Nothing` — the scheduler
skips all sweeps (existing behavior), and offer creation falls through
on tip validation.

### Block MTP for Settlement
REST call to blockspan: `GET /api/v1/oe/blockbyheight/:height`
Config: `BLOCKSPAN_API_URL` (default: `http://127.0.0.1:8999`)

## Scheduler Changes

The existing `schedulerMainLoop` (polls every `configSchedulerPollRateSecs`)
gets two new sweeps alongside the existing expiry sweep:

```
Each tick (when confirmedTip is Just):
  1. Expiry sweep    (existing) — open offers past validTillBlock → expired + refund
  2. Confirming sweep (new)     — accepted offers where targetBlock <= confirmedTip → confirming
  3. Settlement sweep (new)     — confirming offers where targetBlock + 6 <= confirmedTip → settled
```

Order matters: expiry first (frees up offers that shouldn't be accepted),
then confirming, then settlement.

## Config Additions

New keys in the offer service JSON config:

| Key                    | Type   | Default                           |
|------------------------|--------|-----------------------------------|
| `BLOCKSPAN_WS_URL`    | String | `ws://127.0.0.1:8999/api/v1/ws`  |
| `BLOCKSPAN_API_URL`   | String | `http://127.0.0.1:8999`          |
| `PLATFORM_FEE_SATS`   | Int    | `1000`                           |

## File Changes

### Offer API package (`op-energy-offer-api`)

| File | Change |
|------|--------|
| `PostOfferRequest` (new or extend existing) | Add `blockRate :: Double` field |
| `OfferInfo` | Add all new fields (taker, winner, payout, timestamps, blockRate, predictedBlockMTP) |
| `V2/AcceptOfferAPI.hs` | **NEW** — sub-API type for accept endpoint |
| `V2/TradesAPI.hs` | **NEW** — sub-API type for trades listing |
| `V2.hs` | Compose new sub-APIs into `OfferV2API` |

### Offer service package (`op-energy-offer-service`)

| File | Change |
|------|--------|
| `V1/Offer.hs` | Extend Persistent entity with new columns |
| `V1/DB.hs` | Append migration for new columns + PlatformStats table |
| `V1/Config.hs` | Add `configBlockspanWsUrl`, `configBlockspanApiUrl`, `configPlatformFeeSats` |
| `V1/Class.hs` | Add `unconfirmedTip` TVar to State, rename existing to `confirmedTip` |
| `V1/OfferService.hs` | Add `acceptOfferTx` helper (atomic flip) |
| `V1/AccountClient.hs` | No changes (deduct/credit already exist) |
| `V2/AcceptOfferAPI.hs` | **NEW** — wiring module |
| `V2/AcceptOfferAPI/Accept.hs` | **NEW** — handler/logic pair |
| `V2/TradesAPI.hs` | **NEW** — wiring module |
| `V2/TradesAPI/GetTrades.hs` | **NEW** — handler/logic pair |
| `V2/Settlement.hs` | **NEW** — confirming + settlement sweep logic |
| `BlockspanClient.hs` | **NEW** — WebSocket subscriber + REST client for block MTP |
| `Server.hs` | Add blockspan WS thread + settlement sweeps to scheduler |
| `app/Main.hs` | Spawn blockspan WS thread |
| `module-backend.nix` | Add new config options |

### Estimated: ~8 new files, ~7 modified files

## PR Strategy

Single PR on `devel` branch — the settlement engine is one cohesive feature.
All changes are in the offer service (API + service packages). No changes
to the account service needed.

## Out of Scope

- Asymmetric stakes (taker stakes different amount)
- Platform wallet account (fees are burned for now)
- WebSocket notifications to frontend on state changes
- Percentage-based fees
- Real Lightning integration (sandbox only)
