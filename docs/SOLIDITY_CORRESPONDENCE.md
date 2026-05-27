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
| `DOMAIN_SEPARATOR()` | Translated | ECM boundary | Uses Verity static ABI Keccak over domain typehash, chain id, and contract address. |
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
| `setAuthorizationWithSig(Authorization,Signature)` | Translated | Link 1 proven | Nonce ordering and signature-validity abstraction are modeled; EIP-712 hashing uses Verity static ABI/digest ECMs, while ecrecover and raw-log mechanics remain boundaries. |
| `createMarket(MarketParams)` | Translated | Link 1 proven | Static ABI market-id hashing, enabled checks, and state initialization are modeled; event payload log and typed post-create IRM init ECM remain trust boundaries. |
| `setFee(MarketParams,uint256)` | Translated | Link 1 proven | Includes market-created, `ALREADY_SET`, max-fee, accrue-interest, and owner checks. |
| `accrueInterest(MarketParams)` | Translated | Link 1 proven | Interest arithmetic and state updates are modeled; IRM `borrowRate(MarketParams,Market)` uses a typed one-word read ECM and remains an external boundary. |
| `supply(MarketParams,uint256,uint256,address,bytes)` | Translated | Link 1 proven | Accrues interest and updates shares/assets; token behavior and non-flash callback mechanics remain assumptions. |
| `withdraw(MarketParams,uint256,uint256,address,address)` | Translated | Link 1 proven | Accrues interest, checks authorization and liquidity, then transfers assets. |
| `supplyCollateral(MarketParams,uint256,address,bytes)` | Translated | Link 1 proven | Does not accrue interest, matching Solidity; token behavior and callback mechanics remain assumptions. |
| `withdrawCollateral(MarketParams,uint256,address,address)` | Translated | Link 1 proven | Checks authorization/health before collateral transfer; oracle and token behavior remain assumptions. |
| `borrow(MarketParams,uint256,uint256,address,address)` | Translated | Link 1 proven | Accrues interest, checks authorization/health, then transfers borrowed assets; oracle and token behavior remain assumptions. |
| `repay(MarketParams,uint256,uint256,address,bytes)` | Translated | Link 1 proven | Uses zero-floor behavior for total-borrow asset reduction; token behavior and callback mechanics remain assumptions. |
| `liquidate(MarketParams,address,uint256,uint256,bytes)` | Translated | Link 1 proven | Models unhealthy-position check, LIF calculation, bad debt, and transfer order; oracle, callback, and token behavior remain assumptions. |
| `flashLoan(address,uint256,bytes)` | Translated | Link 1 proven | Callback call remains an external trust boundary, but ordering is proven against the pure wrapper model. |

## Cross-Cutting Gates

- `scripts/check_macro_migration_slice.py` verifies the migrated selector
  surface covers every tracked public selector.
- `scripts/check_morpho_event_surface.py` verifies generated event metadata
  against Morpho Blue `EventsLib.sol`.
- `scripts/check_spec_correspondence.py` verifies structural correspondence
  against the legacy `CompilationModel` spec for migrated operations.
- `scripts/check_solidity_correspondence_doc.py` verifies this table stays
  synchronized with macro-migrated semantic bridge obligations.
- `config/semantic-bridge-obligations.json` records which translated operations
  are still conditional on semantic-equivalence hypotheses.
