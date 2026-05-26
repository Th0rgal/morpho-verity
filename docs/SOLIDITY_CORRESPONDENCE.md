# Solidity Correspondence

This table tracks how the current `Morpho/Contract.lean` generated-spec
boundary, backed by the `Morpho/Compiler/MacroSlice.lean` `verity_contract`,
corresponds to Morpho Blue `src/Morpho.sol`.
It is intentionally conservative: "translated" means the macro body has a
recognizable Solidity-facing implementation, not that full Solidity equivalence
has been proved.

## Status Legend

- **Translated**: implemented in `MorphoViewSlice` and covered by the macro
  migration selector slice.
- **Link 1 proven**: the stable `Morpho.*` wrapper API is proved equivalent to
  the EDSL execution for this operation in `SemanticBridgeDischarge.lean`.
- **Assumed boundary**: behavior depends on a named local obligation, generated
  external axiom, or primitive outside the currently proved bridge fragment.

## Public Surface

| Solidity surface | Macro status | Proof/equivalence status | Notes |
|------------------|--------------|--------------------------|-------|
| `DOMAIN_SEPARATOR()` | Translated | Assumed boundary | `domain_separator_memory` covers EIP-712 memory/hash layout. |
| `owner()` | Translated | Read-only generated surface | Reads `ownerSlot`. |
| `feeRecipient()` | Translated | Read-only generated surface | Reads `feeRecipientSlot`. |
| `isIrmEnabled(address)` | Translated | Read-only generated surface | Reads `isIrmEnabledSlot`. |
| `isLltvEnabled(uint256)` | Translated | Read-only generated surface | Reads `isLltvEnabledSlot`. |
| `isAuthorized(address,address)` | Translated | Read-only generated surface | Reads `isAuthorizedSlot`. |
| `nonce(address)` | Translated | Read-only generated surface | Reads `nonceSlot`. |
| `lastUpdate(bytes32)` | Translated | Read-only generated surface | Struct-member read from `marketSlot`. |
| `totalSupplyAssets(bytes32)` | Translated | Read-only generated surface | Struct-member read from `marketSlot`. |
| `totalSupplyShares(bytes32)` | Translated | Read-only generated surface | Struct-member read from `marketSlot`. |
| `totalBorrowAssets(bytes32)` | Translated | Read-only generated surface | Struct-member read from `marketSlot`. |
| `totalBorrowShares(bytes32)` | Translated | Read-only generated surface | Struct-member read from `marketSlot`. |
| `fee(bytes32)` | Translated | Read-only generated surface | Struct-member read from `marketSlot`. |
| `idToMarketParams(bytes32)` | Translated | Read-only generated surface | Returns the five stored market-param words. |
| `position(bytes32,address)` | Translated | Read-only generated surface | Returns supply shares, borrow shares, and collateral. |
| `market(bytes32)` | Translated | Read-only generated surface | Returns all six market fields. |
| `extSloads(bytes32[])` | Translated | Assumed boundary | Dynamic storage-word return behavior remains `returnStorageWords`. |
| constructor | Translated | Not in Link 1 table | Rejects zero owner, sets owner, emits `SetOwner`. |
| `setOwner(address)` | Translated | Link 1 proven | Owner guard and `ALREADY_SET` check modeled. |
| `setFeeRecipient(address)` | Translated | Link 1 proven | Owner guard and `ALREADY_SET` check modeled. |
| `enableIrm(address)` | Translated | Link 1 proven | Owner guard, zero-address, and already-enabled checks modeled. |
| `enableLltv(uint256)` | Translated | Link 1 proven | Owner guard, max-LLTV, and already-enabled checks modeled. |
| `setAuthorization(address,bool)` | Translated | Link 1 proven | `ALREADY_SET` check modeled; `set_authorization_event` covers raw-log encoding. |
| `setAuthorizationWithSig(Authorization,Signature)` | Translated | Assumed boundary | Nonce ordering, EIP-712 hashing, ecrecover, and raw-log mechanics are local obligations. |
| `createMarket(MarketParams)` | Translated | Assumed boundary | Includes enabled checks, storage initialization, event payload log, and post-create IRM init boundary. |
| `setFee(MarketParams,uint256)` | Translated | Assumed boundary | Includes market-created, `ALREADY_SET`, max-fee, accrue-interest, and owner checks. |
| `accrueInterest(MarketParams)` | Translated | Assumed boundary | IRM `borrowRate` remains `irm_borrow_rate_boundary`. |
| `supply(MarketParams,uint256,uint256,address,bytes)` | Translated | Assumed boundary | Accrues interest and updates shares/assets; token/callback mechanics remain assumptions. |
| `withdraw(MarketParams,uint256,uint256,address,address)` | Translated | Assumed boundary | Accrues interest, checks authorization/health, then transfers assets. |
| `supplyCollateral(MarketParams,uint256,address,bytes)` | Translated | Assumed boundary | Does not accrue interest, matching Solidity; token/callback mechanics remain assumptions. |
| `withdrawCollateral(MarketParams,uint256,address,address)` | Translated | Assumed boundary | Checks authorization/health before collateral transfer. |
| `borrow(MarketParams,uint256,uint256,address,address)` | Translated | Assumed boundary | Accrues interest, checks authorization/health, then transfers borrowed assets. |
| `repay(MarketParams,uint256,uint256,address,bytes)` | Translated | Assumed boundary | Uses zero-floor behavior for total-borrow asset reduction; callback/token mechanics remain assumptions. |
| `liquidate(MarketParams,address,uint256,uint256,bytes)` | Translated | Assumed boundary | Models unhealthy-position check, LIF calculation, bad debt, and transfer order. |
| `flashLoan(address,uint256,bytes)` | Translated | Link 1 proven | Callback call remains an external trust boundary, but ordering is proven against the pure wrapper model. |

## Cross-Cutting Gates

- `scripts/check_macro_migration_slice.py` verifies the migrated selector
  surface covers every tracked public selector.
- `scripts/check_morpho_event_surface.py` verifies generated event metadata
  against Morpho Blue `EventsLib.sol`.
- `scripts/check_spec_correspondence.py` verifies structural correspondence
  against the legacy `CompilationModel` spec for migrated operations.
- `config/semantic-bridge-obligations.json` records which translated operations
  are still conditional on semantic-equivalence hypotheses.
