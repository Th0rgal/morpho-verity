# Trust Boundaries

This document inventories the Morpho-specific assumptions the proofs and parity
gates rely on. The repository's claim is implementation fidelity plus empirical
parity against the pinned Morpho Blue target, on top of the two machine-checked
health properties. Everything below is what those claims rest on.

## Artifact Packaging

`morpho-blue-verity/Morpho/Compiler/ArtifactConfig.lean` is not a second contract definition. The
single Morpho source of truth is `morpho-blue-verity/Morpho/Contract.lean`, where
`verity_contract Morpho` produces `Morpho.Contract.Morpho.spec`.

`ArtifactConfig.lean` adapts that macro-produced spec for the standalone compiler
CLI by setting the emitted artifact name, filtering internal helper functions
out of the external selector/ABI surface, and making the linked external
dependency set explicit. That dependency set is empty: `keccakMarketParams`,
`borrowRate`, and `oraclePrice` are implemented in `morpho-blue-verity/Morpho/Contract.lean`
through Verity ECM modules. CI enforces this boundary through
`scripts/check_morpho_artifact_boundary.py`.

## Verity Dependency Pin

The Morpho packages are pinned through `lakefile.lean` and
`lake-manifest.json` to:

- repository: `https://github.com/Th0rgal/verity.git`
- revision: `f064c8260234354a3e4d6f2124d550ec8d15d54f`

This revision brings source-level `keccakString` literals, improved typed
interface support, and — crucially — the `legacyStringSafeTransfer` /
`legacyStringSafeTransferFrom` modules in `Compiler.Modules.ERC20`. These modules
emit the exact Yul that Morpho's `SafeTransferLib` expects (the `extcodesize > 0`
guard and the precise string reverts `"no code"`, `"transfer reverted"`,
`"transfer returned false"`, plus the Solmate-style optional-bool check
`returndatasize > 31 && mload == 1`). An `IERC20` typed interface is declared in
the model alongside `IOracle`, and all transfer call sites now use the Verity
legacy modules via that interface; the old custom `morphoSafeTransfer` /
`morphoSafeTransferFrom` ECMs have been deleted. `scripts/check_verity_pin_sync.py`
enforces that the Lake manifest and lakefile agree on this pin.

## Local Obligations

The macro contract names local obligations at usage sites where Morpho still
crosses a low-level or external boundary:

| Local obligation | Usage |
|------------------|-------|
| `authorization_post_ecrecover_write` | Intentional nonce increment before signature recovery and authorization write after ecrecover, matching Solidity ordering. |
| `create_market_irm_init` | Post-create IRM initialization call in `createMarket`. |
| `supply_callback`, `repay_callback`, `supply_collateral_callback`, `liquidate_callback` | Morpho callback ordering plus token transfer mechanics around the callback boundary. |
| `flash_loan_transfers` | Flash-loan token transfer and callback mechanics. |

## Retained Morpho Blue ECMs

The Blue contract keeps ECMs only where the current source-level surface would
either change Morpho v1 behavior or hide lower-level Solidity mechanics:

| ECM axiom | Solidity construct represented | Why it remains an ECM |
|-----------|--------------------------------|------------------------|
| `optional_callback_target_interface` | `IMorphoSupplyCallback`, `IMorphoRepayCallback`, `IMorphoSupplyCollateralCallback`, and `IMorphoLiquidateCallback` guarded by `data.length > 0`. | The call mechanics are source-ordered and selector-specific, but callback target behavior and dynamic `bytes` forwarding remain an external ABI boundary. |
| `oracle_read_uint256_interface` | `IIrm.borrowRate(...)` / `borrowRateView(...)`-shaped single-word reads. | `IOracle.price()` is now expressed as a typed Verity interface call. IRM calls still use the current Verity helper because the exact Morpho Blue struct ABI selector and mock behavior need a dedicated source-level interface path. |
| Hashing ECM assumptions from `Compiler.Modules.Hashing` | `keccak256(abi.encode(...))` for EIP-712 and `MarketParamsLib.id`. | Static ABI Keccak is source-shaped in the contract but remains a compiler/EVM trust-report boundary until the proof stack models memory-slice Keccak completely. |

**Token transfers** now use Verity's built-in `legacyStringSafeTransfer` /
`legacyStringSafeTransferFrom` modules (with typed `IERC20` interface
parameters). These modules emit the exact Yul that Morpho's `SafeTransferLib`
expects (code-length guard, exact string reverts, optional-bool check), so the
previous custom `morphoSafeTransfer` / `morphoSafeTransferFrom` ECMs have been
removed from the trust boundary.

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

## Verity Coverage

Verity supplies the EVM-shaped primitives the contract builds on: static ABI
Keccak helpers (now also used via the `keccakString` macro for the two EIP-712
typehash constants in the Morpho Blue model), EIP-712 digest helpers, the
legacy-string ERC-20 optional-return modules (`legacyStringSafeTransfer`,
`legacyStringSafeTransferFrom`, `safeApprove`), bubbling call/callback modules,
raw-log validation, and Solidity-0.8 checked-arithmetic helpers. The Morpho
source uses these directly rather than ad hoc stand-ins, so the remaining
trust-report entries stay narrow and auditable.

An `IERC20` typed interface is declared in the model alongside `IOracle`;
all transfer call sites use the Verity `legacyStringSafeTransfer` /
`legacyStringSafeTransferFrom` modules via typed `IERC20` parameters, with the
exact Morpho revert strings and code-length guard. The previous custom
`morphoSafeTransfer` / `morphoSafeTransferFrom` ECMs have been eliminated.

Keccak memory-slice correctness, raw `rawLog` mechanics, low-level call
execution, and precompile correctness are delegated to Verity and can appear in
its trust reports. The fidelity discipline is to keep replacing broad local
obligations with narrow Verity modules, preserve Solidity source ordering around
callbacks and signature recovery, and keep arithmetic translated through checked
operations or explicit guards.

## Refinement Proof Status

The refinement layer assembles generated-body steps from
`morpho-blue-verity/Morpho/Proofs/Disciplines.lean`, but the current Blue
entrypoint-to-projection discipline is still a named Lean boundary rather than a
fully discharged generated-body traversal:

- `supply`, `withdraw`, `supplyCollateral`, and `repay` expose typed
  `MonotoneDiscipline` obligations over the real generated entrypoint runs.
- `withdrawCollateral` and `borrow` expose typed `GuardedDiscipline` obligations
  over the generated `_isHealthy` guard, assuming explicit market-id alignment,
  oracle-price alignment, and `LocalNoOverflowFor`.
- `liquidate` exposes typed obligations for the post-accrual unhealthy guard and
  the no-accrual bridge back to the projected pre-state.
- Health arithmetic now exposes `LocalNoOverflowFor` as the exact four-field
  domain required by `healthFaithful_of_noOverflow`: `totalBorrowAssets + 1`,
  `totalBorrowShares + 1000000`, watched collateral times oracle price, and
  quoted collateral times LLTV must fit in `uint256`. `Disciplines.lean` proves
  `noOverflow_of_localNoOverflow` from that local domain instead of assuming it.
  The borrow-side share/asset conversion uses the full-precision `mulDiv512Up`
  helper, and its quotient-fit proof is derived from the packed `uint128`
  storage reads at the `healthFaithful_of_noOverflow` call site.
- `StorageFrame.lean` still isolates the keccak/layout slot-injectivity fact as
  the single direct storage-frame axiom.

## Current Gates

- `lake build` validates Lean elaboration and internal consistency for the
  currently imported implementation files.
- `scripts/check_morpho_event_surface.py` checks contract event metadata
  against Morpho Blue `EventsLib.sol`.
- `scripts/check_morpho_artifact_boundary.py` checks that artifact packaging
  stays thin and any linked externals keep stable axiom names.
- `scripts/run_morpho_blue_parity.sh` runs the empirical Solidity-vs-Verity
  differential suite for the pinned Morpho Blue checkout.
