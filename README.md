# How to read this document

This document is using Mermaid plugin to define diagrams. The easiest way to view such diagrams is to access README from github, which automatically renders Mermaid diagrams in place.
Another way is to copy-past the content into https://stackedit.io

# Architecture Diagrams

## Use case diagram:

Use case diagram shows use cases of the system

```mermaid
C4Context

title Use case diagram

Person(user, "Op-Energy user", "user that is using op-energy")
Boundary(b1, "blockspan-service", "boundary") {
  System(usecase0, "view blockspan information", "")
  System(usecase1, "views strike summary (Past)", "")
  System(usecase2, "views strike summary (Future)", "")
  System(usecase3, "views strike detail (Past)", "")
  System(usecase4, "views strike detail (Future)", "")
  System(usecase5, "views energy summary (Past)", "")
  System(usecase6, "views energy summary (Future)", "")
  System(usecase7, "views energy detail (Past)", "")
  System(usecase8, "views energy detail (Future)", "")
}
Boundary(b2, "account-service", "boundary") {
  System(usecase201, "registers itself in op-energy system", "")
  System(usecase202, "logins itself in op-energy system", "")
}
Boundary(b3, "blocktime-service(currently builtin to account service)", "boundary") {
  System(usecase10, "creates future blocktime strikes", "")
  System(usecase11, "views future blocktime strikes", "")
  System(usecase12, "creates guesses for a future blocktime strikes", "")
  System(usecase13, "views guesses for a future blocktime strikes", "")
  System(usecase14, "views observation results for past blocktime strikes", "")
}

Rel(user, usecase0, "")
Rel(user, usecase1, "")
Rel(user, usecase2, "")
Rel(user, usecase3, "")
Rel(user, usecase4, "")
Rel(user, usecase5, "")
Rel(user, usecase6, "")
Rel(user, usecase7, "")
Rel(user, usecase8, "")
Rel(user, usecase10, "")
Rel(user, usecase11, "")
Rel(user, usecase12, "")
Rel(user, usecase13, "")
Rel(user, usecase14, "")
Rel(user, usecase201, "")
Rel(user, usecase202, "")
```

## Highlevel overview of Op-Energy

This diagram shows high level overview of Op-Energy system

NOTE: email notification service is present, but not implemented yet

```mermaid
C4Context

title Highlevel overview of Op-Energy

Person(user, "Op-Energy user", "user that is using op-energy")
Boundary(b1, "op-energy frontend", "boundary") {
  System(OpEnergyFrontend, "Op-Energy frontend", "")
}
Boundary(b2, "op-energy backend", "boundary") {
  System(OpEnergyBackend, "Op-Energy backend", "")
}
Boundary(b3, "bitcoin", "boundary") {
  System(Bitcoin, "Bitcoin node", "")
}
Boundary(b4, "notificatons", "boundary") {
  System(EMail, "email service", "")
}
Rel( user, OpEnergyFrontend, "")
Rel( OpEnergyFrontend, OpEnergyBackend, "")
Rel( OpEnergyBackend, Bitcoin, "")
Rel( OpEnergyBackend, EMail, "")
```

## Backend view of component diagram

This part describes backend-centric components diagram

```mermaid
C4Context

title Backend Context diagram for Op-Energy

Person(userA, "Op-Energy user", "user that is using op-energy to view information about blocks")
Person_Ext(userD, "user that is not registered in op-energy")

Boundary(b1, "op-energy", "boundary") {

    Boundary(frontendBoundary, "op-energy frontend", "boundary") {
      System(OeFrontend, "op-energy frontend")
    }
    Boundary(backendBoundary, "op-energy backend") {
        Boundary(b4, "oe-blockspan-service", "boundary") {
            System(OeBlockspanApi, "oe-blockspan-api", "An op-energy API to operate with blockspans service")
            System(OeBlockspanService, "oe-blockspan-service", "An op-energy system that operates with blockspans")
            SystemDb(OeBlockspanServiceDB, "openergy")
        }
        Boundary(b5, "oe-account-service", "boundary") {
            System(OeAccountApi, "oe-account-api", "An op-energy API to operate with account service")
            System(OeAccountService, "oe-account-service", "An op-energy system that operates with accounts")
            SystemDb(OeAccountServiceDB, "openergyacc")
        }
        Boundary(b6, "oe-blocktime-service", "boundary") {
            System(OeBlocktimeApi, "oe-blocktime-api", "An op-energy API to operate with blocktime service")
            System(OeBlocktimeService, "oe-blocktime-service", "An op-energy system that operates with blocktime strikes")
        }


        Boundary(b7, "Bitcoin", "boundary") {
            System_Ext(btcNode, "BTC node", "bitcoin mainnet node")
            SystemDb(mainnet, "mainnet", "blockchain")
        }
    }

}

Rel(btcNode,mainnet, "")
Rel(userA, OeFrontend, "Uses")

Rel(userD, OeFrontend, "views graphs")
Rel( OeFrontend, OeBlockspanService, "gets data for graphs")

Rel(userD, OeFrontend, "views blockspan reports graphs")
Rel( OeFrontend, OeBlockspanService, "gets data for blockspan reports")

Rel( OeBlockspanService, OeBlockspanServiceDB, "Postgres")
Rel( OeBlocktimeService, OeAccountServiceDB, "Postgres")
Rel( OeBlocktimeService, OeBlockspanService, "Websockets")
Rel( OeAccountService, OeAccountServiceDB, "Postgres")


Rel_D(OeFrontend, OeBlockspanService, "reads block chain info", "Blockspan API, websockets")
Rel_D(OeFrontend, OeAccountService, "", "Account API")
Rel_D(OeFrontend, OeBlocktimeService, "", "Blocktime API")
Rel_U(OeBlockspanService,btcNode, "reads block chain info", "JSON-RPC")
```

## Frontend view of component diagram

```mermaid
C4Context

title Frontend view of component diagram for Op-Energy

Person(userA, "Op-Energy user", "registered user that is using op-energy")
Person_Ext(userD, "user that is not registered in op-energy")

Boundary(b1, "op-energy", "boundary") {

    Boundary(frontendBoundary, "op-energy frontend", "boundary") {
      System(OeFrontendDocs, "frontend docs ui component")
      System(OeFrontendLogin, "frontend login component")
      System(OeFrontendRegister, "frontend login component")
      System(OeFrontendBlockrateGraph, "frontend blockrate graph component")
      System(OeFrontendBlockspans, "frontend blockspans component")
      System(OeFrontendEnergyDetail, "frontend energy-detail component")
      System(OeFrontendEnergySummary, "frontend energy-summary component")
      System(OeFrontendStrikeDetail, "frontend strike-detail component")
      System(OeFrontendStrikeSummary, "frontend strike-summary component")
      System(OeFrontendiBlocktimeStrikeComponent, "frontend blocktime strike component")
      System(OeFrontendiBlocktimeStrikeGuessComponent, "frontend blocktime strike guess component")

      System(OeFrontendState, "frontend state service")
      System(OeFrontendWebsocket, "frontend websockets service")
    }
    Boundary(backendBoundary, "op-energy backend") {
      System(OeBackend, "op-energy-backend", "An op-energy system that operates with blockspans, accounts, strikes and so on")
    }

}

Rel(userD, OeFrontendDocs, "views swagger specs", "Blockspan API, Account API, Blocktime API")
Rel(userD, OeFrontendRegister, "registers himself")
Rel(userD, OeFrontendLogin, "logins himself")
Rel(userD, OeFrontendBlockrateGraph, "views graphs")
Rel(userD, OeFrontendBlockspans, "views Blockspan reports", "")
Rel(userA, OeFrontendEnergyDetail, "views energy details", "")
Rel(userA, OeFrontendEnergySummary, "views energy summary", "")
Rel(userA, OeFrontendStrikeDetail, "views strike details", "")
Rel(userA, OeFrontendStrikeSummary, "views strike summary", "")
Rel(userA, OeFrontendiBlocktimeStrikeComponent, "creates/views blocktime strikes", "")
Rel(userA, OeFrontendiBlocktimeStrikeGuessComponent, "creates/views blocktime strike guesses", "")

Rel( OeFrontendDocs, OeBackend, "gets swagger specs", "HTTP API")
Rel( OeFrontendRegister, OeBackend, "registers user", "Account HTTP API")
Rel( OeFrontendLogin, OeBackend, "logins user", "Account HTTP API")
Rel( OeFrontendBlockrateGraph, OeBackend, "gets data for graphs", "Blockspan HTTP API")
Rel( OeFrontendBlockspans, OeBackend, "gets data for blockspan reports", "Blockspan HTTP API")
Rel( OeFrontendEnergyDetail, OeBackend, "gets data for energy details", "Blockspan HTTP API")
Rel( OeFrontendEnergySummary, OeBackend, "views energy summary", "Blockspan HTTP API")
Rel( OeFrontendStrikeDetail, OeBackend, "gets data for strike details", "Blockspan HTTP API")
Rel( OeFrontendStrikeSummary, OeBackend, "views strike summary", "Blockspan HTTP API")
Rel( OeFrontendiBlocktimeStrikeComponent, OeBackend, "creates/views blocktime strikes", "Blocktime HTTP API")
Rel( OeFrontendiBlocktimeStrikeComponent, OeFrontendState, "uses account token", "")
Rel( OeFrontendiBlocktimeStrikeGuessComponent, OeBackend, "creates/views blocktime strike guesses", "Blocktime HTTP API")
Rel( OeFrontendiBlocktimeStrikeGuessComponent, OeFrontendState, "uses account token", "Blocktime HTTP API")

Rel( OeFrontendRegister, OeFrontendState, "stores account token", "")
Rel( OeFrontendLogin, OeFrontendState, "stores account token", "")

Rel( OeFrontendWebsocket, OeBackend, "receives latest confirmed blocks events", "Websocket")
Rel( OeFrontendWebsocket, OeFrontendState, "stores latest confirmed blocks events", "Websocket")
```

## GIT repos diagram

This part contains diagram of git repositories being used in op-energy.
It should be noted that:
1. the entry point of API libraries are usually `$GIT_REPO/src/Data/OpEnergy/$SERVICE/API.hs`. This module defines prefix route (for example, `/api/v1/account/`) and then includes modules, that defines subroutes (for example, module `$GIT_REPO/src/Data/OpEnergy/Account/API/V1.hs` defines subroutes `register` and `login` and thus `/api/v1/account/register` and `/api/v1/account/login` are being defined). Each such library add `/api/v1/$SERVICE/swagger.json` handler, such that implementations should serve swagger spec of appropriate API. Each API library can be built as binary, which  just outputs Swagger API spec in json format;
2. each service, that implements appropriate API, are binary executables and their entrypoint is `$GIT_REPO/app/Main.hs`, which calls Servant library. But, all the services share some servicing part and thus function `$GIT_REPO/src/OpEnergy/$SERVICE/Server.hs/runServer` can be assumed as a real entrypoint of an appropriate service. This module includes subsequent modules that implement appropriate APIs;
3. for a sake of simplicity, currently Account API, Blocktime API are merged into 1 library;
4. for a sake of simplicity, currently Account Service and Blocktime Service are merged into 1 library;
5. Swagger specs are being generated automatically from API specificatons by using servant-swagger library. Check `https://docs.servant.dev/en/stable/tutorial/ApiType.html` for a tutorial of how to define API.

```mermaid
C4Context

title Repo components diagram

Container(OeFrontend, "https://github.com/girish-velas/ope-blockspan-service/frontend", "Frontend components")
Container(OeBlockspanAPI, "https://github.com/girish-velas/ope-blockspan-service/op-energy-api", "Blockspan API")
Container(OeBlockspanService, "https://github.com/girish-velas/ope-blockspan-service/op-energy-backend", "Blockspan Service")

Boundary(b1, "Implementation merged for a sake of simplicity", "boundary") {
  Container(OeAccountAPI, "https://github.com/op-energy-foundation/op-energy/oe-account-service/op-energy-account-api/src/Data/OpEnergy/Account/API.hs", "Account API")
  Container(OeAccountService, "https://github.com/op-energy-foundation/op-energy/oe-account-service/op-energy-account-service/src/OpEnergy/Account/Server.hs", "Account Service")
  Container(OeBlocktimeAPI, "https://github.com/op-energy-foundation/op-energy/oe-account-service/op-energy-account-api/src/Data/OpEnergy/Account/API.hs", "Blocktime API")
  Container(OeBlocktimeService, "https://github.com/op-energy-foundation/op-energy/oe-account-service/op-energy-account-service/src/OpEnergy/BlockTime/Server.hs", "Blocktime Service")
}
```

## Sequence diagrams

### bootstrap sequence

This diagram shows starting up process of op-energy

```mermaid
sequenceDiagram
  rect rgb(247,247,240)
    note left of OS: blockspan service boot
    OS ->>+ blockspan service: blockspan service boot
    blockspan service ->>+ openergy DB: read all the records from blockheader table
    openergy DB ->>- blockspan service: blockheader
    blockspan service ->> blockspan service: store blockheader into the cache
    blockspan service ->> blockspan service: assume latest confirmed block = last read blockheader
    blockspan service ->>+ bitcoin node: get blockchain info
    bitcoin node ->>- blockspan service: blockchain info
    blockspan service ->> blockspan service: assume current tip = latest block in blockchain - 6 blocks
    blockspan service ->>+ bitcoin node: for each (current tip - lastest confirmed block): do get block header
    bitcoin node ->>- blockspan service: block header
    blockspan service ->> openergy DB: add blockheader record
    blockspan service ->> blockspan service: store blockheader in  the cache
    blockspan service ->> blockspan service: store latest blockheader as current tip header
    blockspan service ->>+ blockspan scheduler thread : start scheduler
    loop every Config.SCHEDULER_POLL_RATE_SECS
      blockspan scheduler thread ->>+ bitcoin node: get blockchain info
      bitcoin node ->>- blockspan scheduler thread: blockchain info
      blockspan scheduler thread ->>+ bitcoin node: foreach [ confirmed tip header + 1 .. blockchain info tip - 6]: get block header
      bitcoin node ->>- blockspan scheduler thread: block header
      blockspan scheduler thread ->> blockspan service: store new latest confirmed tip = blockchain block header - 6
    end
    deactivate blockspan scheduler thread
    blockspan service ->> blockspan service: start HTTP listener
    blockspan service ->>- blockspan service: handle requests
  end

  rect rgb(247,247,240)
    note right of blocktime service: starting websocket thread
    OS ->>+ blocktime service: start service
    blocktime service ->>+ blocktime websocket thread: start websocket connection
    blockspan service ->> blocktime websocket thread: init message
    loop when new confirmed tip discovered
      blockspan service --> blocktime websocket thread: latest confirmed block header
      blocktime websocket thread ->>- blocktime websocket thread: store confirmed block header
    end
    deactivate blocktime service
  end
```

###  blockspan reports or graphs

This diagram shows the process of blockspan reports or graphs

```mermaid
sequenceDiagram
    user ->>+ frontend: request blockspan report/graph etc
    activate frontend
    frontend ->>+ blockspan service: request blockspan(s)/blockheader(s)
    alt when blockspan > lastest confirmed block
      blockspan service --> frontend: error HTTP 404
      frontend --x user: show error
    else
      blockspan service ->> blockspan service: lookup blockheader(s) <= confirmed tip in the cache
      blockspan service ->>- frontend: blockspan(s)/blockheader(s)
      frontend ->>- user: blockspan(s)/blockheader(s) report
    end
```
###  user registration

This diagram shows the process of user registration

```mermaid
sequenceDiagram
    user ->>+ frontend: navigates to op-energy not with /login/ subpath
    frontend ->>+ account service: register()
    loop while insertion in DB raises exception
      account service ->> account service: generate UUID, secret and name
      account service ->> openergyacc DB: store new user record( UUID, hashed secret)
    end
    account service ->>- frontend: return secret and account token
    frontend ->> frontend: store account token into runtime state service
    frontend ->> frontend: navigates to the main page
    frontend ->>- user: show message "Your account login URL is <URL>"
```

###  user login

This diagram shows the process of user login

```mermaid
sequenceDiagram
    user ->>+ frontend: navigates to op-energy with /login/$secret subpath
    frontend ->>+ account service: login(secret)
    account service ->> openergyacc DB: lookup existing account with hashed secret
    alt when user does not exist
      account service ->> frontend: error HTTP 404
      frontend --x user: show error
    else
      account service ->> openergyacc DB: update seen time
      account service ->>- frontend: return account token
      frontend ->> frontend: store account token into runtime state service
      frontend ->>- user: navigates to the main page
    end
```

###  block time strike creation

This diagram shows the process of block time strike creation

```mermaid
sequenceDiagram
    user ->>+ frontend: creates strike defined by (block height, nLockTime)
    frontend ->> frontend: get account token from local state
    frontend ->>+ blocktime service: create strike defined by (account token, block height, nLockTime)
    blocktime service ->>+ account service: verify(account token)
    alt when user is not verified
      account service ->> blocktime service: verification failure
      blocktime service ->> frontend: error HTTP 404
      frontend --x user: show error
    else
      account service ->>- blocktime service: user account record
    end
    alt when (blockheight <= last confirmed block - Config.configBlockTimeStrikeMinimumBlockAheadCurrentTip) or nlocktime is in the past
      blocktime service --x frontend: error HTTP 404
    else
      blocktime service ->> openergyacc DB: store (block height, nLockTime) as a new block time strike in the DB
      blocktime service ->>- frontend: HTTP 200
      frontend ->>- user: show message "created"
    end
```

### automatic future block time strike creation

Blocktime service provides automaticall creation of future strikes during handling latest confirmed block event. See oe-account-service/op-energy-account-service/src/OpEnergy/BlockTimeStrike/Server/V1/BlockTimeScheduledFutureStrikeCreation.hs for implementation. Current implementation ensures, that there are future strike exist in range `[ minimumFutureStrikeHeight, maximumFutureStrikeHeight]`, where:

```haskell
  minimumFutureStrikeHeight = blockHeaderHeight currentTip + configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip
  maximumFutureStrikeHeight = blockHeaderHeight currentTip + configBlockTimeFutureStrikeShouldExistsAheadCurrentTip
```

```mermaid
sequenceDiagram
    blockspan service -> blocktime service: current tip event message (websocket)
    blocktime service ->> blocktime service: ensureFutureStrikeExistsAhead(currentTip)
```

### viewing list of block time strikes

```mermaid
sequenceDiagram
    user ->>+ frontend: get list of block time strikes
    frontend ->> frontend: get account token from local state
    frontend ->>+ blocktime service: listBlocktimeStrikes(account token)
    blocktime service ->>+ account service: verify(account token)
    alt when no user found
      account service ->> blocktime service: verification failure
      blocktime service ->> frontend: error HTTP 404
      frontend --x user: show error
    else
      account service ->>- blocktime service: user account record
      blocktime service ->>+ openergyacc DB: get list of block time strikes
      openergyacc DB ->>- blocktime service: list of block time strikes
      blocktime service ->>- frontend: list of block time strikes
      frontend ->>- user: list of block time strikes
    end
```

### create block time strike's guess

```mermaid
sequenceDiagram
    user ->>+ frontend: create block time strike's guess defined by (block height, nlocktime, guess)
    frontend ->> frontend: get account token from local state
    frontend ->>+ blocktime service: createBlockTimeStrikeGuess(account token, block height, nlocktime, guess)
    blocktime service ->>+ account service: verify(account token)
    alt when no user found
      account service ->> blocktime service: verification failure
      blocktime service ->> frontend: error HTTP 404
      frontend --x user: show error
    else
      account service ->>- blocktime service: user account record
      blocktime service ->> openergyacc DB: store block time strike's guess (account token, block height, nlocktime, guess)
      blocktime service ->>- frontend: HTTP 200
      frontend ->>- user: show message 'created'
    end
```

# Implementation details

This section containes some implementation details, that were considered as important enough to note

## Blockspan service

### BlockHeader cache

Blockspan service implements immutable cache of the BlockHeaders for the whole block chain. It's purpose is to provide constant access to any BlockHeader.
On a high level overview, this cache is implemented as IOVector( Compact( Vector BlockHeader)) and thus:
1. implementation provides constant access time;
2. reduces load on garbage collector by using Compact regions.
You can check implementation in $ope-blockspan-repo/op-energy-backend/src/OpEnergy/Server/V1/BlockHeadersService/Vector/Cache.hs

## Op-energy account service

1. AccountToken's payload is currently implemented as tuple (uuid, loginsCount) encoded into JSON then encryped with AES (see Web.ClientSession from clientsession library) and then encoded into base64 form. Where:
 - uuid is of type UUID Person, which is unique string;
 - loginsCount - is of type Word64 and it is a monotonically increasing integer.
The reasoning behind such payload content is that it allowes us to consider AccountTokens to be valid only when loginsCount in the token matches loginsCounts field in DB record. It is supposed, that loginsCount DB record field will
l be updated each time user logins in with 'login' API call.
This way we are able to ensure, that:
2. each user will get unique tokens within a range of [ 0; MAX Word64 value ];
3. only 1 unique token is assumed to be valid;
4. there is no possibility to use old leaked token to access any users other than owner of such token;
5. when user will overflow MAX Word64 value of logins counts, he will start to get repeating tokens

Current AccountToken implementation's goal is to address those attack vectors:
1. secret leak. reduced possibility of leak by requiring it only once for login;
2. token leak. reduced damage by invalidating previous tokens at each new login;
3. using tokens to access other users accounts. Tokens are only valid for an owner of the token.
4. token modification. In order to modify token, an attacker should know encryption key which is being used by backend.

## Configuration files

Each service has configuration file. You can define path to a config by using appropriate environment variables:
- blockspan service: "OPENERGY_BACKEND_CONFIG_FILE";
- account service: "OPENERGY_ACCOUNT_SERVICE_CONFIG_FILE";

blocktime service now is a part of account service and thus it shares account service's environment variable for configuration file.

Configuration options defined in:
- blockspan service: ope-blockspan-service/op-energy-backend/src/OpEnergy/Server/V1/Config.hs;
- account service: op-energy/oe-account-service/op-energy-account-service/src/OpEnergy/Account/Server/V1/Config.hs.

