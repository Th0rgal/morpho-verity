# Trust Boundaries

This document records the current Morpho-specific assumptions that remain after
the `00c18e3a` Verity upgrade, which includes upstream Verity PR
`lfglabs-dev/verity#1939`. It is an inventory, not a proof
claim: each item must either be discharged, replaced by a concrete Verity
module, or remain an explicit assumption before stronger Solidity-equivalence
claims are made.

## Generated Externals

`Morpho/Compiler/Generated.lean` currently carries no linked externals. Former
placeholders for `keccakMarketParams`, `borrowRate`, and `oraclePrice` have been
replaced in `Morpho/Compiler/MacroSlice.lean` with Verity ECM modules. CI
enforces this boundary through `scripts/check_morpho_generated_boundary.py`.

## Local Obligations

The macro contract also names local obligations at usage sites when the current
Verity proof fragment does not yet cover the mechanics:

| Local obligation | Usage |
|------------------|-------|
| `set_authorization_event` | `SetAuthorization` raw-log memory encoding in `setAuthorization`. |
| `authorization_post_ecrecover_write` | Intentional nonce increment before signature recovery and authorization write after ecrecover, matching Solidity ordering. |
| `create_market_irm_init` | Post-create IRM initialization call in `createMarket`. |
| `supply_callback`, `repay_callback`, `supply_collateral_callback`, `liquidate_callback` | Morpho callback ordering plus token transfer mechanics around the callback boundary. |
| `flash_loan_transfers` | Flash-loan token transfer and callback mechanics. |

## Still Assumed At The Current Pin

- ERC-20 optional-return SafeTransferLib target behavior, token no-fee/no-reentrancy
  economic assumptions, and external callee liveness.
- ECDSA/ecrecover cryptographic correctness.
- Static ABI/EIP-712/Keccak memory-slice correctness as surfaced by Verity ECM
  trust reports.
- Callback ECM target behavior, including the flash-loan callback between
  `safeTransfer` and `safeTransferFrom`.
- Event/log memory mechanics for raw-log paths, including `CreateMarket`'s
  encoded `MarketParams` payload.
- Low-level returndata and revert bubbling for external calls.
- Links 2+3 compilation correctness are delegated to Verity's compiler
  framework, as tracked in `docs/EQUIVALENCE_OBLIGATIONS.md`.

## Verity Coverage Audit

At the current Morpho pin (`00c18e3a`), several EVM-shaped features that were
previously represented through local obligations or linked externals are now
directly actionable: the framework exposes
static ABI Keccak helpers, EIP-712 digest helpers, Solmate-compatible ERC-20
optional-return ECMs, bubbling call/callback modules, raw-log validation tests,
and Solidity-0.8 checked-arithmetic helpers.

That means the next fidelity work is mostly in Morpho:

- replace generic placeholders with the new Verity modules,
- add Morpho-specific typed ECMs for IRM and oracle calls,
- preserve Solidity source ordering around callbacks and signature recovery,
- replace broad arithmetic assumptions with checked operations or local guards,
- and discharge the remaining repo-local Link 1 proofs.

Keccak memory-slice correctness, raw `rawLog` mechanics, low-level call
execution, and precompile correctness can still appear in Verity trust reports,
but Morpho no longer needs to invent ad hoc stand-ins for these Solidity
constructs. The Morpho source should use the upstream primitives so the
remaining report entries are narrow and auditable.

## Current Gates

- `lake build` validates that generated event declarations are internally
  consistent with macro bodies that use `emit`.
- `scripts/check_morpho_event_surface.py` checks generated event metadata
  against Morpho Blue `EventsLib.sol`.
- `scripts/check_morpho_generated_boundary.py` checks that generated externals
  keep stable axiom names.
- `scripts/check_primitive_coverage.py` reports which EDSL primitives still sit
  outside the proven bridge fragment.

## Post-Verity #1939 Actionability

After Morpho advanced its Verity pin to `00c18e3a`, the following boundaries are
Morpho-local cleanup work rather than missing Verity features:

- `keccakMarketParams`: replace the linked external with
  `Compiler.Modules.Hashing.abiEncodeStaticWords` over the exact five
  `MarketParams` words.
- `DOMAIN_SEPARATOR()` and `setAuthorizationWithSig`: build the EIP-712 domain,
  authorization struct hash, and final digest with the new static ABI and
  `eip712Digest` helpers. Keep only `ecrecover` cryptographic correctness as an
  EVM/precompile assumption.
- ERC-20 transfer helpers: use `Compiler.Modules.ERC20.solmateSafeTransfer` and
  `solmateSafeTransferFrom`, matching Morpho Blue's Solmate `SafeTransferLib`
  optional-return semantics.
- Callback and low-level-call paths: use the upstream callback and bubbling-call
  modules so calldata layout, free-memory-pointer handling, and revert-data
  bubbling are source-level translated instead of broadly assumed.
- IRM and oracle calls: introduce typed ECMs with exact selector/calldata,
  staticcall/call kind, one-word return decoding, and returndata bubbling.
- Events: prefer `emit` for fixed events and retain raw logs only where the
  Solidity source uses manual/dynamic payload construction, with exact topic and
  payload-size tests.
- Arithmetic: translate Solidity 0.8 checked operations with Verity's checked
  panic helpers or explicit guards, then retire operation-level overflow
  hypotheses as the individual proofs are discharged.

The durable assumptions after those changes should be external behavior:
oracle/IRM truthfulness and liveness, token economics, gas-sensitive behavior,
and EVM primitive/precompile correctness. The detailed worklist is in
[`POST_VERITY_1939_MORPHO_ACTIONS.md`](POST_VERITY_1939_MORPHO_ACTIONS.md).
