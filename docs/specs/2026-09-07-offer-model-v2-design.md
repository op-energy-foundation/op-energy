# Offer Data Model V2 — Design Spec

## Context

The current backend offer model stores a flat set of fields per offer row:
`offerId`, `creatorDisplayName`, `targetBlock`, `validTillBlock`,
`makerStakeSats`, `status`, `expiresAt`, `refundedAt`, `created`.

When a maker posts `numberOfOffers = 5`, the backend creates **5 separate DB
rows**. There is no concept of contracts, matching, settlement, or the
financial model the prototype UI expects.

The prototype UI (https://exchange.op-energy.info/prototype/) and its source
(https://github.com/op-energy-foundation/op-energy-prototype) define the
target data model. This spec aligns the backend to match.

## Goals

1. Replace the "N separate rows" model with a **group model**: one offer row =
   one listing with `totalContracts` and `matchedCount`.
2. Add missing fields: `mtpCutoffEpoch`, `side`, `takerStakeSats`, `blockRate`,
   `createdAtBlock`.
3. Introduce a **Contract** entity for individual matched trades.
4. Add platform fee as a backend constant.
5. Add settlement and expiry logic hooks (actual settlement is a later task).

## Data Model

### Offer (one row = one offer group)

```
Offer
  personUUID          UUID Person
  creatorDisplayName  DisplayName
  targetBlock         Natural Int        -- block height being bet on
  mtpCutoffEpoch      Word64             -- predicted MTP in unix seconds
  side                OfferSide          -- maker's side: BEFORE or AFTER
  validTillBlock      Natural Int        -- offer expires after this block
  makerStakeSats      Word64             -- sats the maker risks per contract
  takerStakeSats      Word64             -- sats the taker risks per contract
  blockRate           Double             -- min/block assumption (e.g. 10.0)
  totalContracts      Natural Int        -- how many contracts in this offer
  matchedCount        Natural Int        -- how many have been taken
  createdAtBlock      Natural Int        -- block height at creation time
  status              OfferStatus        -- Open | Filled | Expired | Cancelled
  expiresAt           UTCTime Maybe      -- set when expired
  refundedAt          UTCTime Maybe      -- set when refunded
  created             UTCTime            -- creation timestamp
```

**Notes:**
- `takerStakeSats = TOTAL_POT - makerStakeSats`. Stored explicitly so the
  frontend doesn't need to know the pot constant.
- `side` is typed as `"BEFORE" | "AFTER"`. In practice the form always
  submits `"BEFORE"` but the backend accepts both.
- `matchedCount` increments each time a taker accepts.
- `status` transitions: `Open → Filled` (when `matchedCount == totalContracts`),
  `Open → Expired` (when `validTillBlock` passes), `Open → Cancelled` (maker
  cancels unfilled portion). A partially-matched offer that gets cancelled
  reduces `totalContracts` to `matchedCount` and sets status to `Filled`.

### OfferSide (new enum)

```haskell
data OfferSide = Before | After
```

Serialized as `"BEFORE"` / `"AFTER"` in JSON. Stored as `SqlString` in DB.

### OfferStatus (updated)

```haskell
data OfferStatus = Open | Filled | Expired | Cancelled
```

Replaces the current 6-variant enum. `Accepted`, `Confirming`, `Settled` move
to the Contract entity.

### Contract (new entity — one row per taker acceptance)

```
Contract
  offerId             OfferId            -- FK to parent offer
  targetBlock         Natural Int        -- denormalized from offer
  mtpCutoffEpoch      Word64             -- denormalized from offer
  makerSide           OfferSide          -- denormalized from offer
  makerUUID           UUID Person        -- offer creator
  makerDisplayName    DisplayName        -- denormalized
  makerStakeSats      Word64             -- denormalized from offer
  takerUUID           UUID Person        -- the taker
  takerDisplayName    DisplayName        -- the taker's name
  takerStakeSats      Word64             -- denormalized from offer
  blockRate           Double             -- denormalized from offer
  status              ContractStatus     -- Live | Settled
  actualMtpEpoch      Word64 Maybe       -- actual MTP once target block settles
  winnerSide          OfferSide Maybe    -- BEFORE or AFTER, set on settlement
  createdAtBlock      Natural Int        -- block height when matched
  matchedAt           UTCTime            -- when the taker accepted
  settledAt           UTCTime Maybe      -- when settlement resolved
```

**Notes:**
- Fields are denormalized from the parent Offer so that contract listings
  don't require joins.
- `status` is only `Live` or `Settled`. The "confirming" state (1-5 of 6
  confirmations) is computed at query time from `currentTip - targetBlock + 1`
  and returned in the API response, not stored.
- Settlement rule: `actualMtp < mtpCutoffEpoch` → maker wins (BEFORE),
  `actualMtp >= mtpCutoffEpoch` → taker wins (AFTER).

### ContractStatus (new enum)

```haskell
data ContractStatus = Live | Settled
```

### Constants

```haskell
-- | Total sats in every contract pot (makerStake + takerStake).
totalPotSats :: Word64
totalPotSats = 100_000

-- | Platform fee deducted from the pot. Winner receives
-- totalPotSats - platformFeeSats = 99,000.
platformFeeSats :: Word64
platformFeeSats = 1_000

-- | Confirmations required before settlement.
settlementConfirmations :: Int
settlementConfirmations = 6
```

These live in a `Constants.hs` module within the offer service. The frontend
hardcodes the same values in a config file. No API endpoint for constants —
if they change, update both backend and frontend together.

## API Changes

All existing routes are preserved — no new routers. The response shapes are
updated, and one new sub-route is added under the existing offer API type.

### Existing Routes (5) — Updated Response Shapes

#### 1. POST /api/v1/offer/post (updated request + response)

```
POST /api/v1/offer/post
Authorization: <token>

{
  "targetBlock": 966032,
  "mtpCutoffEpoch": 1725800400,
  "side": "BEFORE",
  "validTillBlock": 966012,
  "makerStakeSats": 25000,
  "blockRate": 10.0,
  "totalContracts": 3,
  "createdAtBlock": 965952
}
```

`takerStakeSats` is computed server-side: `totalPotSats - makerStakeSats`.

**Validation:**
- `makerStakeSats` must be 1 to `totalPotSats - 1` (so takerStake ≥ 1)
- `totalContracts` must be 1 to 20
- `targetBlock` must be > current tip
- `validTillBlock` must be > current tip and < `targetBlock`
- `side` must be `"BEFORE"` or `"AFTER"`
- `blockRate` must be > 0
- User balance must be ≥ `makerStakeSats * totalContracts`

**Response:** `PostOfferResult { offers: [OfferInfo] }` (single-element array
since one row = one offer group now).

#### 2. GET /api/v1/offer/list (updated response)

```
GET /api/v1/offer/list?status=open&page=1&limit=20
```

Response shape updated to include both offers and contracts:

```json
{
  "offers": [OfferInfo, ...],
  "contracts": [ContractInfo, ...],
  "page": 1,
  "limit": 20,
  "totalCount": 30
}
```

Existing query params (`status`, `creatorDisplayName`, `page`, `limit`) filter
the offers. Contracts are returned alongside — all live/settled contracts for
the offers on the current page.

#### 3. GET /api/v1/offer/mine (updated response)

```
GET /api/v1/offer/mine
Authorization: <token>
```

Response updated to return both:

```json
{
  "offers": [OfferInfo, ...],
  "contracts": [ContractInfo, ...]
}
```

`offers` = offers created by this user.
`contracts` = contracts where this user is maker or taker. Each contract
includes `yourRole: "maker" | "taker"` computed from the auth token.

#### 4. GET /api/v1/offer/:id (updated response)

```
GET /api/v1/offer/:id
```

Returns `OfferInfo` with the new fields.

#### 5. POST /api/v1/offer/:id/cancel (updated behavior)

```
POST /api/v1/offer/:id/cancel
Authorization: <token>
```

**Updated behavior:**
- If `matchedCount == 0`: full cancel. Refund `makerStakeSats * totalContracts`.
  Set status to `Cancelled`.
- If `matchedCount > 0`: partial cancel. Set `totalContracts = matchedCount`,
  refund `makerStakeSats * (original - matchedCount)`. Set status to `Filled`.
  Existing contracts remain `Live`.

### New Route (1) — Accept Offer

```
POST /api/v1/offer/:id/accept
Authorization: <token>

Response: ContractInfo
```

Added as a sub-route under the existing offer API type, alongside the existing
`/:id/cancel` route.

**Flow:**
1. Verify offer exists, status is `Open`, and `matchedCount < totalContracts`.
2. Verify taker is not the offer creator (can't take your own offer).
3. Deduct `takerStakeSats` from taker's balance.
4. Create a `Contract` row with `status = Live`.
5. Increment `offer.matchedCount`.
6. If `matchedCount == totalContracts`, set offer status to `Filled`.
7. Return the created `ContractInfo`.

### OfferInfo (updated response type)

```json
{
  "offerId": "42",
  "creatorDisplayName": "brave_tiger_482",
  "targetBlock": 966032,
  "mtpCutoffEpoch": 1725800400,
  "side": "BEFORE",
  "validTillBlock": 966012,
  "makerStakeSats": 25000,
  "takerStakeSats": 75000,
  "blockRate": 10.0,
  "totalContracts": 3,
  "matchedCount": 1,
  "createdAtBlock": 965952,
  "status": "open",
  "expiresAt": null,
  "refundedAt": null,
  "created": "2026-09-07T12:00:00Z"
}
```

### ContractInfo (new response type)

```json
{
  "contractId": "17",
  "offerId": "42",
  "targetBlock": 966032,
  "mtpCutoffEpoch": 1725800400,
  "makerSide": "BEFORE",
  "makerDisplayName": "brave_tiger_482",
  "makerStakeSats": 25000,
  "takerDisplayName": "cool_river_831",
  "takerStakeSats": 75000,
  "blockRate": 10.0,
  "status": "live",
  "confirmations": 0,
  "actualMtpEpoch": null,
  "winnerSide": null,
  "yourRole": "taker",
  "createdAtBlock": 965955,
  "matchedAt": "2026-09-07T12:05:00Z",
  "settledAt": null
}
```

`confirmations` is computed at query time: `max(0, currentTip - targetBlock + 1)`
when `status == Live`, or `6` when `status == Settled`.
`yourRole` is included when the request has an auth token, omitted otherwise.

## DB Migration

The offer service has a versioned migration system (`offerDBMigrations` in
`DB.hs`, currently empty). This change requires:

### Migration 0 → 1: Alter offer table + create contract table

1. **Add columns** to `offer` table:
   - `mtp_cutoff_epoch BIGINT NOT NULL DEFAULT 0`
   - `side VARCHAR NOT NULL DEFAULT 'BEFORE'`
   - `taker_stake_sats BIGINT NOT NULL DEFAULT 50000`
   - `block_rate DOUBLE PRECISION NOT NULL DEFAULT 10.0`
   - `total_contracts INTEGER NOT NULL DEFAULT 1`
   - `matched_count INTEGER NOT NULL DEFAULT 0`
   - `created_at_block INTEGER NOT NULL DEFAULT 0`

2. **Update existing rows**: set `total_contracts = 1`, `matched_count = 0`,
   `taker_stake_sats = 100000 - maker_stake_sats`, and status mappings:
   - `accepted` → update offer status to `filled`, create a contract row
   - `confirming` → keep offer as `filled`, create a contract row with `Live`
   - `settled` → keep offer as `filled`, create a contract row with `Settled`
   - `open` → keep as `open`
   - `expired` → keep as `expired`
   - `cancelled` → keep as `cancelled`

3. **Create `contract` table** with all fields from the Contract entity above.

4. **Update OfferDB version** to 1.

## Frontend Changes (op-energy-mvp-dev)

### Updated types in `offerApi.ts`

```typescript
export type OfferSide = "BEFORE" | "AFTER";
export type OfferStatus = "open" | "filled" | "expired" | "cancelled";
export type ContractStatus = "live" | "settled";

export interface OfferInfo {
  offerId: string;
  creatorDisplayName: string;
  targetBlock: number;
  mtpCutoffEpoch: number;
  side: OfferSide;
  validTillBlock: number;
  makerStakeSats: number;
  takerStakeSats: number;
  blockRate: number;
  totalContracts: number;
  matchedCount: number;
  createdAtBlock: number;
  status: OfferStatus;
  expiresAt: string | null;
  refundedAt: string | null;
  created: string;
}

export interface ContractInfo {
  contractId: string;
  offerId: string;
  targetBlock: number;
  mtpCutoffEpoch: number;
  makerSide: OfferSide;
  makerDisplayName: string;
  makerStakeSats: number;
  takerDisplayName: string;
  takerStakeSats: number;
  blockRate: number;
  status: ContractStatus;
  confirmations: number;
  actualMtpEpoch: number | null;
  winnerSide: OfferSide | null;
  yourRole?: "maker" | "taker";
  createdAtBlock: number;
  matchedAt: string;
  settledAt: string | null;
}
```

### Constants (hardcoded config — no API)

```typescript
// src/config/constants.ts
export const PLATFORM = {
  TOTAL_POT_SATS: 100_000,
  PLATFORM_FEE_SATS: 1_000,
  WINNER_PAYOUT_SATS: 99_000,
  SETTLEMENT_CONFIRMATIONS: 6,
  MAX_CONTRACTS: 20,
  MIN_STAKE_SATS: 1,
  MAX_STAKE_SATS: 99_999,
} as const;
```

Same values hardcoded in both backend (`Constants.hs`) and frontend
(`constants.ts`). If they change, update both together.

### Updated API functions in `offerApi.ts`

```typescript
// Updated — response now includes contracts
listOffers(filters?): Promise<{ offers: OfferInfo[], contracts: ContractInfo[], page, limit, totalCount }>
getMyOffers(): Promise<{ offers: OfferInfo[], contracts: ContractInfo[] }>

// New — only new endpoint
acceptOffer(offerId: string): Promise<ContractInfo>

// Unchanged
postOffer(params: PostOfferParams): Promise<PostOfferResult>
getOffer(offerId: string): Promise<OfferInfo>
cancelOffer(offerId: string): Promise<OfferInfo>
```

### Updated PostOfferParams

```typescript
export interface PostOfferParams {
  targetBlock: number;
  mtpCutoffEpoch: number;
  side: OfferSide;
  validTillBlock: number;
  makerStakeSats: number;
  blockRate: number;
  totalContracts: number;
  createdAtBlock: number;
}
```

`numberOfOffers` renamed to `totalContracts`. `takerStakeSats` removed from
request (computed server-side).

## Settlement (future task — not in this scope)

Settlement requires:
1. A background job that watches the chain tip.
2. When `tip >= contract.targetBlock + settlementConfirmations`:
   fetch actual MTP for `targetBlock` from blockspan service.
3. Apply rule: `actualMtp < mtpCutoffEpoch` → maker wins, else taker wins.
4. Credit winner with `totalPotSats - platformFeeSats`.
5. Update contract: `status = Settled`, `actualMtpEpoch`, `winnerSide`,
   `settledAt`.

This is a separate task because it requires integration with the blockspan
service and a background worker.

## Offer Expiry (future task — not in this scope)

A background job checks `validTillBlock < currentTip` for `Open` offers:
1. Refund `makerStakeSats * (totalContracts - matchedCount)`.
2. If `matchedCount > 0`: set `totalContracts = matchedCount`, status = `Filled`.
3. If `matchedCount == 0`: status = `Expired`, set `expiresAt`.

## Verification

### Backend
- `POST /api/v1/offer/post` with new fields → returns `OfferInfo` with all
  fields populated, `takerStakeSats = 100000 - makerStakeSats`.
- `POST /api/v1/offer/:id/accept` → creates contract, increments
  `matchedCount`, returns `ContractInfo`.
- `GET /api/v1/offer/list` → returns `{ offers, contracts, page, limit,
  totalCount }`.
- `GET /api/v1/offer/mine` → returns `{ offers, contracts }` with `yourRole`
  on each contract.
- `POST /api/v1/offer/:id/cancel` with `matchedCount > 0` → partial cancel,
  refund unfilled portion.

### Frontend
- `npm run build` passes with updated types.
- PostOfferPanel sends new fields.
- Constants hardcoded in `src/config/constants.ts`.
