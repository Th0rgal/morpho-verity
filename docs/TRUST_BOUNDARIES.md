# Trust Boundaries

This document records the current Morpho-specific assumptions that remain after
the `b699e300` Verity upgrade. It is an inventory, not a proof claim: each item
must either be discharged, replaced by a concrete Verity module, or remain an
explicit assumption before stronger Solidity-equivalence claims are made.

## Generated Externals

`Morpho/Compiler/Generated.lean` names every linked external in the generated
compiler boundary. CI enforces these names through
`scripts/check_morpho_generated_boundary.py`.

| External | Axiom name | Current role |
|----------|------------|--------------|
| `keccakMarketParams` | `market_id_deterministic` | Placeholder for `MarketParamsLib.id`; must become exact `keccak256(abi.encode(marketParams))` over the five-word tuple. |
| `borrowRate` | `irm_borrow_rate_boundary` | IRM `borrowRate(marketParams, market)` environment boundary; return value, no-revert behavior, and no-reentrancy remain assumed. |
| `collateralPrice` | `oracle_collateral_price_boundary` | Legacy oracle-price adapter boundary retained for parity with the old spec surface. |
| `oraclePrice` | `oracle_price_boundary` | Oracle `price()` environment boundary; scale, bounds, no-revert behavior, and freshness remain assumed. |

## Local Obligations

The macro contract also names local obligations at usage sites when the current
Verity proof fragment does not yet cover the mechanics:

| Local obligation | Usage |
|------------------|-------|
| `domain_separator_memory` | `DOMAIN_SEPARATOR()` memory writes and EIP-712 domain hash layout. |
| `set_authorization_event` | `SetAuthorization` raw-log memory encoding in `setAuthorization`. |
| `authorization_sig_memory` | EIP-712 typed-data hashing plus ecrecover memory layout in `setAuthorizationWithSig`. |
| `authorization_post_ecrecover_write` | Intentional nonce increment before signature recovery and authorization write after ecrecover, matching Solidity ordering. |
| `create_market_irm_init` | Post-create IRM initialization call in `createMarket`. |
| `supply_callback`, `repay_callback`, `supply_collateral_callback`, `liquidate_callback` | Morpho callback ordering plus token transfer mechanics around the callback boundary. |
| `flash_loan_transfers` | Flash-loan token transfer and callback mechanics. |

## Still Assumed

- ERC-20 optional-return SafeTransferLib behavior, token no-fee/no-reentrancy
  economic assumptions, and external callee liveness.
- ECDSA/ecrecover cryptographic correctness and exact EIP-712 digest layout.
- Callback ECM target behavior, including the flash-loan callback between
  `safeTransfer` and `safeTransferFrom`.
- Event/log memory mechanics for raw-log paths, including `CreateMarket`'s
  encoded `MarketParams` payload.
- Low-level returndata and revert bubbling for external calls.
- Links 2+3 compilation correctness are delegated to Verity's compiler
  framework, as tracked in `docs/EQUIVALENCE_OBLIGATIONS.md`.

## Verity Coverage Audit

The current Verity pin has important compiler and differential-testing support
for Morpho-shaped EVM features, but those features are not all in the same
proof-complete fragment as simple storage transitions. The upstream
`docs/INTERPRETER_FEATURE_MATRIX.md`, `docs/EXTERNAL_CALL_MODULES.md`, and
`docs/ARITHMETIC_PROFILE.md` show the following coverage gaps:

- Fully proving `keccak256(abi.encode(...))` needs stronger first-class
  ABI/memory-slice Keccak support. Current Verity can compile/use Keccak through
  hashing helpers and direct memory slices, but
  `keccak256_memory_slice_matches_evm` remains a trust boundary.
- Fully proving low-level calls, returndata, revert bubbling, optional ERC-20
  bool returns, and callbacks needs Verity's low-level call/returndata proof
  interpreter support to mature. The standard ECMs model the call choreography
  and surface assumptions, but `call`, `returndataCopy`, `revertReturndata`, and
  ECM execution are still proof-boundary features.
- Fully proving EIP-712 needs first-class typed-data / ABI-encoding helpers plus
  memory Keccak proofs. `ecrecover` cryptographic correctness will always remain
  an EVM/precompile assumption.
- Raw-log/event memory equivalence, especially dynamic/manual event payloads
  like `CreateMarket`, needs Verity event/memory proof support beyond simple
  `emit`. `rawLog` and general linear-memory mechanics remain outside the
  current proof interpreter coverage.
- Global Solidity 0.8 checked-arithmetic equivalence would benefit from
  Verity-level checked arithmetic syntax/lowering/proof automation, though local
  guards can be added in Morpho now. Verity currently exposes explicit
  `safeAdd`/`safeSub`/`safeMul`/`safeDiv` constructs; it does not insert
  Solidity-style overflow checks for bare arithmetic automatically.

## Current Gates

- `lake build` validates that generated event declarations are internally
  consistent with macro bodies that use `emit`.
- `scripts/check_morpho_event_surface.py` checks generated event metadata
  against Morpho Blue `EventsLib.sol`.
- `scripts/check_morpho_generated_boundary.py` checks that generated externals
  keep stable axiom names.
- `scripts/check_primitive_coverage.py` reports which EDSL primitives still sit
  outside the proven bridge fragment.
