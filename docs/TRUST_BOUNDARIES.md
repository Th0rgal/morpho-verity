# Trust Boundaries

This document records the current Morpho-specific assumptions that remain after
the `00c18e3a` Verity upgrade, which includes upstream Verity PR
`lfglabs-dev/verity#1939`. It is an inventory, not a proof claim.

The current repository claim is implementation fidelity plus empirical parity
against the pinned Morpho Blue target. It is not a complete formal
invariant-proof claim for Morpho.

## Generated Externals

`Morpho/Compiler/Generated.lean` currently carries no linked externals. Former
placeholders for `keccakMarketParams`, `borrowRate`, and `oraclePrice` have been
replaced in `Morpho/Contract.lean` with Verity ECM modules. CI
enforces this boundary through `scripts/check_morpho_generated_boundary.py`.

## Local Obligations

The macro contract names local obligations at usage sites where Morpho still
crosses a low-level or external boundary:

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
- Compilation correctness is delegated to Verity's compiler framework and the
  pinned artifact/parity gates in this repository.

## Verity Coverage Audit

At the current Morpho pin (`00c18e3a`), several EVM-shaped features that were
previously represented through local obligations or linked externals are now
directly actionable: the framework exposes
static ABI Keccak helpers, EIP-712 digest helpers, Solmate-compatible ERC-20
optional-return ECMs, bubbling call/callback modules, raw-log validation tests,
and Solidity-0.8 checked-arithmetic helpers.

That means the next fidelity work is mostly in Morpho: keep replacing broad
local obligations with narrow Verity modules, preserve Solidity source ordering
around callbacks and signature recovery, and keep arithmetic translated through
checked operations or explicit guards. Any new Morpho-specific invariant proofs
should be rebuilt as a separate layer over `verity_contract Morpho`; the current
documents and CI should not be read as claiming that layer exists.

Keccak memory-slice correctness, raw `rawLog` mechanics, low-level call
execution, and precompile correctness can still appear in Verity trust reports,
but Morpho no longer needs to invent ad hoc stand-ins for these Solidity
constructs. The Morpho source should use the upstream primitives so the
remaining report entries are narrow and auditable.

## Current Gates

- `lake build` validates Lean elaboration and internal consistency for the
  currently imported implementation files.
- `scripts/check_morpho_event_surface.py` checks generated event metadata
  against Morpho Blue `EventsLib.sol`.
- `scripts/check_morpho_generated_boundary.py` checks that generated externals
  keep stable axiom names.
- `scripts/run_morpho_blue_parity.sh` runs the empirical Solidity-vs-Verity
  differential suite for the pinned Morpho Blue checkout.
