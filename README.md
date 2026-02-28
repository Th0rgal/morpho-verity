# Morpho Verity

Formal verification of [Morpho Blue](https://github.com/morpho-org/morpho-blue) using [Verity](https://github.com/Th0rgal/verity), a Lean 4 framework for verified smart contracts.

## Goal

Audit Morpho Blue by building an equivalent implementation in Lean 4 and proving it correct.

The approach: translate Morpho's Solidity logic line-by-line into Verity's contract DSL, state the key safety properties as formal specs, then prove them mechanically. A successful proof gives stronger guarantees than traditional auditing — if the translation is faithful, bugs in the spec are bugs in the original.

### What this proves

- **Solvency**: total borrows never exceed total supply, preserved by all 16 state-mutating operations (supply, withdraw, borrow, repay, liquidate, accrueInterest, supplyCollateral, withdrawCollateral, createMarket, setFee, and all admin functions)
- **Rounding safety**: all share/asset conversions round against the user; round-trip supply-withdraw never returns more than deposited
- **Authorization**: only authorized addresses can withdraw, borrow, or remove collateral; liquidation requires an unhealthy position; signature-based delegation requires valid nonce and unexpired deadline
- **Fee bounds**: market fees stay within the 25% cap
- **Collateralization**: positions with debt always have collateral, preserved by all 16 operations including borrow and withdrawCollateral (guarded by health checks; bad debt is socialized by liquidation)
- **Monotonicity**: enabled IRMs/LLTVs cannot be disabled across all operations; market timestamps only increase through accrueInterest, setFee, and accrueInterestPublic
- **Exchange rate safety**: supply share exchange rate never decreases after interest accrual (accrueInterest and accrueInterestPublic); existing shareholders' per-share value is protected
- **Market isolation**: operations on one market never affect any other market's state, same-market user positions, or any position in other markets
- **Share accounting**: totalSupplyShares = sum of individual supplyShares and totalBorrowShares = sum of individual borrowShares, preserved by all 17 operations (supply, withdraw, borrow, repay, liquidate, accrueInterest, supplyCollateral, withdrawCollateral, createMarket, setFee, enableIrm, enableLltv, setOwner, setFeeRecipient, setAuthorization, setAuthorizationWithSig, accrueInterestPublic)

### What this does not prove

The Lean implementation targets logical equivalence with Morpho's Solidity, not bytecode equivalence. The compiled Yul output will differ. External call behavior (oracle prices, IRM rates, ERC20 transfers, EIP-712 signature verification) is modeled as parameters, not verified end-to-end.

### Proof Boundaries (Proved vs Assumed)

| Area | Current status | Gate/condition |
|------|----------------|----------------|
| Lean invariants/specs | Proved in this repo | `lake build` succeeds |
| Solidity equivalence transfer | Conditional | Per-operation semantic equivalence obligations must be discharged |
| Verity artifact parity | Empirical/differential today | Pinned parity target + Yul identity gate in CI |
| External dependencies (oracle/token/signature env) | Assumed model inputs | Explicit trust assumptions and scenario matrix |

Groundwork docs for closing these gaps:
- [`docs/PARITY_TARGET.md`](docs/PARITY_TARGET.md)
- [`docs/EQUIVALENCE_OBLIGATIONS.md`](docs/EQUIVALENCE_OBLIGATIONS.md)
- [`docs/RELEASE_CRITERIA.md`](docs/RELEASE_CRITERIA.md)

Machine-readable parity target artifacts:
- [`config/parity-target.json`](config/parity-target.json)
- [`config/yul-identity-unsupported.json`](config/yul-identity-unsupported.json)
- [`scripts/check_parity_target.py`](scripts/check_parity_target.py)
- [`scripts/report_yul_identity_gap.py`](scripts/report_yul_identity_gap.py)

Some theorems are conditional on arithmetic side conditions (`h_no_overflow`) that model Solidity checked arithmetic.
These are explicit theorem hypotheses today, not globally discharged reachability facts.

### Solidity Equivalence Bridge

`Morpho/Proofs/SolidityBridge.lean` adds 46 proof-transfer theorems for core invariants.
This file is a conditional transfer layer: it assumes operation-by-operation semantic equivalence hypotheses and then transports proved invariants.
If a Solidity/Yul semantics model is shown equivalent to each corresponding Verity operation
(`supply`, `withdraw`, `borrow`, `repay`, `supplyCollateral`, `withdrawCollateral`, `liquidate`, `accrueInterest`, `enableIrm`, `enableLltv`, `setAuthorization`, `setAuthorizationWithSig`),
the existing Verity proofs for `borrowLeSupply`, `alwaysCollateralized`, `irmMonotone`, and `lltvMonotone` transfer directly to Solidity.

## Key Modeling Notes

- `createMarket` now derives `id = marketId(params)` inside the transition (matching Solidity), so callers cannot provide arbitrary ids.
- `marketId` is no longer a constant placeholder; it is a deterministic function of `MarketParams`.
- Interest accrual remains a compositional model (`accrueInterest` can be called explicitly by clients/proofs), while Solidity calls it internally in several entrypoints.

## Structure

```
morpho-blue/              # Morpho Blue Solidity (git submodule)
Morpho/
  Types.lean              # MarketParams, Position, Market, MorphoState, Authorization
  Morpho.lean             # Core logic: supply, withdraw, borrow, repay, liquidate, setAuthorizationWithSig
  Libraries/
    MathLib.lean          # WAD arithmetic (mulDivDown/Up, wMulDown, wTaylorCompounded)
    SharesMathLib.lean    # Share/asset conversion with virtual offset
    UtilsLib.lean         # exactlyOneZero, min, zeroFloorSub
    ConstantsLib.lean     # MAX_FEE, ORACLE_PRICE_SCALE, LIQUIDATION_CURSOR
  Specs/
    Invariants.lean       # Formal property definitions
    Rounding.lean         # Rounding direction specs
    Authorization.lean    # Access control specs
  Proofs/
    Invariants.lean       # Invariant proofs (105 proven, 98 public + 7 helper lemmas)
    Rounding.lean         # Rounding proofs (4/4 proven)
    Authorization.lean    # Authorization proofs (13 proven, 11 public + 2 helper lemmas)
    SolidityBridge.lean   # Solidity equivalence bridge proofs (46/46 proven)
    ShareConsistency.lean # Share accounting proofs (36 proven, 34 public + 2 helper lemmas)
    NatListSum.lean       # List sum lemmas for share accounting (5/5 proven)
  Compiler/
    Spec.lean             # Morpho Blue contract specification (CompilationModel DSL)
    Main.lean             # Yul codegen patches for Solidity storage/event compatibility
compiler/
  external-libs/          # External Yul libraries (MarketParamsHash, etc.)
  yul/                    # Generated Yul output
verity-foundry/           # Foundry project for Verity-compiled artifact testing
scripts/
  prepare_verity_morpho_artifact.sh  # Build + compile Verity Morpho artifact
  run_morpho_blue_parity.sh          # Run full Morpho Blue suite (Solidity vs Verity)
```

## Build

Requires [Lean 4](https://leanprover.github.io/lean4/doc/setup.html) (v4.22.0).

```
lake build
```

## Test

Initialize the Morpho Blue submodule (required for Solidity tests and parity suite):

```bash
git submodule update --init --recursive
```

Run Solidity Morpho Blue tests (default deployment path):

```bash
cd morpho-blue
forge test -vvv
```

Run the exact same Morpho Blue suite against both implementations (Solidity and Verity-compiled):

```bash
./scripts/run_morpho_blue_parity.sh
```

CI can skip the internal parity preflight in this script by setting
`MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1` because parity is already enforced in
dedicated lanes. Outside CI, this skip is fail-closed unless explicitly
overridden with `MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP=1`.
`MORPHO_VERITY_SKIP_PARITY_PREFLIGHT`,
`MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP`, and
`MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP` are fail-closed toggles and must be
set to `0` or `1`.
Even in skip mode, artifact preparation stays fail-closed behind the shared
timeout wrapper via `MORPHO_VERITY_PREP_TIMEOUT_SEC` (default `900`).
`run_morpho_blue_parity.sh` also fails closed if the parity/prep stage does not
produce non-empty `Morpho.yul`, `Morpho.bin`, and `Morpho.abi.json` artifacts.
The full differential suite can also be fail-closed with
`MORPHO_BLUE_SUITE_TIMEOUT_SEC` (default `0`, disabled).
`MORPHO_BLUE_SUITE_TIMEOUT_SEC` must be a non-negative integer.
When this guard is enabled, `timeout` must be available in `PATH`.

Validate pinned parity tuple (solc + Foundry profile):

```bash
python3 scripts/check_parity_target.py
```

Generate Solidity-vs-Verity Yul identity report artifacts:

```bash
python3 scripts/report_yul_identity_gap.py
```

Validate that current function-level Yul gaps match the tracked unsupported manifest:

```bash
python3 scripts/report_yul_identity_gap.py --enforce-unsupported-manifest
```

Build the Morpho Verity artifact:

```bash
./scripts/prepare_verity_morpho_artifact.sh
```

Artifact preparation is fail-closed for invalid toggle values and missing required tooling:
- `MORPHO_VERITY_SKIP_BUILD` / `MORPHO_VERITY_SKIP_SOLC` must be `0` or `1`.
- `MORPHO_VERITY_INPUT_MODE` must be `model` or `edsl`.
- `python3` is required to read `config/parity-target.json` when present.
- `config/parity-target.json` must include a non-empty `verity.parityPackId`.
- `compiler/external-libs/MarketParamsHash.yul` must be present.
- `lake` is required for compiler build/exec.
- `solc` and `awk` are required unless `MORPHO_VERITY_SKIP_SOLC=1`.

Generate only Yul + ABI (skip `solc` bytecode generation):

```bash
MORPHO_VERITY_SKIP_SOLC=1 ./scripts/prepare_verity_morpho_artifact.sh
```

Select compiler input boundary mode explicitly (`edsl` default, transitionally routed via manual bridge):

```bash
MORPHO_VERITY_INPUT_MODE=model ./scripts/prepare_verity_morpho_artifact.sh
```

Override artifact output directory when needed:

```bash
MORPHO_VERITY_OUT_DIR=/tmp/morpho-artifact ./scripts/prepare_verity_morpho_artifact.sh
```

Enforce transition-boundary parity (`model` vs `edsl`) for generated Morpho artifacts (`Morpho.yul`, `Morpho.bin`, `Morpho.abi.json`):

```bash
./scripts/check_input_mode_parity.sh
```

The parity gate is fail-closed: `Morpho.yul` and `Morpho.bin` must match byte-for-byte, and `Morpho.abi.json` must match semantically (canonical JSON).
ABI canonicalization requires `python3`; missing or invalid ABI JSON artifacts fail closed with deterministic diagnostics.
Artifact preparation is also fail-closed with a timeout guard (`MORPHO_VERITY_PREP_TIMEOUT_SEC`, default `900`; set `0` to disable). When this guard is enabled, `timeout` must be available in `PATH`.
Workflow long-lane commands also use fail-closed timeout guards via a shared timeout wrapper:
- `MORPHO_LEAN_INSTALL_TIMEOUT_SEC` (default `600`)
- `MORPHO_FOUNDRY_INSTALL_TIMEOUT_SEC` (default `600`)
- `MORPHO_SOLC_INSTALL_TIMEOUT_SEC` (default `600`)
- `MORPHO_VERITY_PREP_TIMEOUT_SEC` (default `900`)
- `MORPHO_YUL_IDENTITY_TIMEOUT_SEC` (default `1500`)
- `MORPHO_SOLIDITY_TEST_TIMEOUT_SEC` (default `5100`)
- `MORPHO_VERITY_SMOKE_TIMEOUT_SEC` (default `3000`)
- `0` disables timeout for each respective command

Compile using a specific Verity parity pack:

```bash
MORPHO_VERITY_PARITY_PACK=solc-0.8.28-o999999-viair-true-evm-paris \
  ./scripts/prepare_verity_morpho_artifact.sh
```

Run Foundry smoke tests on the compiled Verity artifact:

```bash
cd verity-foundry
forge test -vvv
```

Current status:
- The full Morpho Blue Foundry suite is wired to run against both implementations via `MORPHO_IMPL=solidity|verity` in `morpho-blue/test/BaseTest.sol`.
- `./scripts/run_morpho_blue_parity.sh` runs that exact suite for both targets and stores logs in `out/parity/`.
- Differential pass/fail depends on the currently checked-out `morpho-blue` submodule revision; use the logs under `out/parity/` as the source of truth for a given run.
- `scripts/report_yul_identity_gap.py` emits machine-readable identity artifacts under `out/parity-target/` (`report.json` + `normalized.diff`) including structural-AST mismatch localization (top-level + function-level, with token line/column coordinates), name-insensitive function-body pairing diagnostics (`functionBlocks.nameInsensitivePairs`), deterministic mismatch family grouping (`functionBlocks.familySummary`) to prioritize rewrite families, and an enforced unsupported-manifest drift gate (including parity-target ID match).

## Proof progress

**209 theorems proven, 0 sorry remaining.**

| Category | Proven | Total | Status |
|----------|--------|-------|--------|
| Invariants | 105 | 105 | Done |
| Solidity equivalence bridge | 46 | 46 | Done |
| Share consistency | 36 | 36 | Done |
| Authorization | 13 | 13 | Done |
| List sum lemmas | 5 | 5 | Done |
| Rounding | 4 | 4 | Done |

Also proven in supporting libraries:
- `mulDivDown_le_mulDivUp` — floor division ≤ ceiling division (MathLib)
- `zeroFloorSub_le` — saturating subtraction never exceeds original (UtilsLib)
- `u256_val` — simp lemma for Uint256 wrapping arithmetic

Invariant theorems (105) include:
- Solvency (borrowLeSupply) preserved by all 16 operations + accrueInterestPublic (17 public + 3 helper lemmas)
- Collateralization preserved by all 16 operations (16 public + 3 helper lemmas)
- IRM monotonicity preserved by 14 operations including accrueInterestPublic (14)
- LLTV monotonicity preserved by 14 operations including accrueInterestPublic (14)
- Market isolation for 8 operations: accrueInterest/supply/withdraw/borrow/repay/liquidate/supplyCollateral/withdrawCollateral (8)
- Same-market position isolation for 8 operations (8)
- Cross-market position isolation for 8 operations (8)
- Timestamp monotonicity for accrueInterest/setFee/accrueInterestPublic (3)
- Exchange rate monotonicity for accrueInterest/accrueInterestPublic (2 public + 1 helper lemma)
- LLTV < WAD (1), market creation validity (1), fee bounds (1)
- Flash loan rejects zero assets (1), accrueInterestPublic rejects uninitialized markets (1)
- accrueInterestPublic preserves solvency and collateralization (2)

Solidity equivalence bridge theorems (46) include:
- borrowLeSupply preservation for 10 operations: supply/withdraw/borrow/repay/supplyCollateral/withdrawCollateral/liquidate/accrueInterest/enableIrm/enableLltv + setAuthorization/setAuthorizationWithSig (12)
- alwaysCollateralized preservation for the same 12 operations (12)
- irmMonotone preservation for 12 operations (12)
- lltvMonotone preservation for 10 operations (10)

Authorization theorems (13) include:
- Precondition style: unauthorized sender → withdraw/borrow/withdrawCollateral return none (3)
- Postcondition style: successful withdraw/borrow/withdrawCollateral → sender was authorized (3)
- Supply requires no authorization (1)
- Liquidation requires unhealthy position (1)
- Signature-based: expired deadline rejected, wrong nonce rejected, nonce incremented (3)
- Helper lemmas: isSenderAuthorized characterization (2)

Share consistency theorems (36) include:
- supplySharesConsistent preserved by all 17 operations (17)
- borrowSharesConsistent preserved by all 17 operations (17)
- Helper lemmas: supply/borrow consistency propagation (2)

## Status

- [x] Morpho types and state model
- [x] Core contract logic (supply, withdraw, borrow, repay, liquidate, supplyCollateral, withdrawCollateral, createMarket, setAuthorization, setAuthorizationWithSig, owner functions, interest accrual, flash loans)
- [x] Math libraries (MathLib, SharesMathLib, UtilsLib, ConstantsLib)
- [x] Formal specs with human-readable documentation (invariants, rounding, authorization)
- [x] Authorization proofs (13: withdraw/borrow/withdrawCollateral require auth, supply doesn't, postcondition specs, liquidation requires unhealthy, signature validation, helper lemmas)
- [x] Invariant proofs (105: solvency × 17, collateralization × 16, IRM/LLTV monotonicity × 14 each, market/position isolation × 24, timestamp × 3, exchange rate × 2, standalone checks, helper lemmas)
- [x] Rounding proofs (4/4: toSharesDown ≤ toSharesUp, toAssetsDown ≤ toAssetsUp, supply round-trip protocol-safe, withdraw round-trip protocol-safe)
- [x] Share consistency proofs (36: supplySharesConsistent and borrowSharesConsistent preserved by all 17 operations including liquidate bad-debt socialization, helper lemmas)
- [x] Solidity equivalence bridge proofs (46: borrowLeSupply/alwaysCollateralized/irmMonotone/lltvMonotone preserved across 10-12 operations via semantic equivalence transfer)

## License

MIT
