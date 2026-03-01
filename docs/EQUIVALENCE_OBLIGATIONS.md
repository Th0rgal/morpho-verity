# Equivalence Obligations (Groundwork)

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document tracks the bridge assumptions that must become proved lemmas to support stronger Solidity-equivalence claims.

## Status

Groundwork only. Tracking format is proposed; current bridge assumptions are still assumptions.

## Scope

`Morpho/Proofs/SolidityBridge.lean` currently transfers invariants under semantic-equivalence hypotheses.
Each hypothesis must be tracked as a proof obligation with owner and status.

## Proposed Obligation Table

| Obligation ID | Bridge hypothesis | Operation | Owner | Status |
|---------------|-------------------|-----------|-------|--------|
| `OBL-SUPPLY-SEM-EQ` | `supplySemEq` | `supply` | `TBD` | `assumed` |
| `OBL-WITHDRAW-SEM-EQ` | `withdrawSemEq` | `withdraw` | `TBD` | `assumed` |
| `OBL-BORROW-SEM-EQ` | `borrowSemEq` | `borrow` | `TBD` | `assumed` |
| `OBL-REPAY-SEM-EQ` | `repaySemEq` | `repay` | `TBD` | `assumed` |
| `OBL-SUPPLY-COLLATERAL-SEM-EQ` | `supplyCollateralSemEq` | `supplyCollateral` | `TBD` | `assumed` |
| `OBL-WITHDRAW-COLLATERAL-SEM-EQ` | `withdrawCollateralSemEq` | `withdrawCollateral` | `TBD` | `assumed` |
| `OBL-LIQUIDATE-SEM-EQ` | `liquidateSemEq` | `liquidate` | `TBD` | `assumed` |
| `OBL-ACCRUE-INTEREST-SEM-EQ` | `accrueInterestSemEq` | `accrueInterest` | `TBD` | `assumed` |
| `OBL-ENABLE-IRM-SEM-EQ` | `enableIrmSemEq` | `enableIrm` | `TBD` | `assumed` |
| `OBL-ENABLE-LLTV-SEM-EQ` | `enableLltvSemEq` | `enableLltv` | `TBD` | `assumed` |
| `OBL-SET-AUTH-SEM-EQ` | `setAuthorizationSemEq` | `setAuthorization` | `TBD` | `assumed` |
| `OBL-SET-AUTH-SIG-SEM-EQ` | `setAuthorizationWithSigSemEq` | `setAuthorizationWithSig` | `TBD` | `assumed` |

## Semantic Bridge Discharge Path

The obligations above will be discharged via the Verity semantic bridge
([verity#998](https://github.com/Th0rgal/verity/issues/998),
[verity#1052](https://github.com/Th0rgal/verity/pull/1052)).

The discharge requires a three-layer correspondence for each operation:

```
Morpho.f args                        -- pure Lean model (this repo)
  = MorphoViewSlice.f.exec state     -- EDSL macro output (verity_contract)
  = EVMYulLean(compile(spec)).exec   -- verified EVM semantics (verity bridge)
```

- **Link 1** (pure Lean ↔ EDSL): requires that `MorphoViewSlice` functions in
  `MacroSlice.lean` have full implementations matching `Morpho.*`. Currently,
  simple view functions are fully implemented; complex operations (supply,
  borrow, liquidate, etc.) are stub/noop — these must be completed as macro
  primitive support grows in verity.

- **Link 2** (EDSL ↔ EVMYulLean): provided by the verity semantic bridge once
  Layers 2+3 are composed into per-function theorems. This eliminates the
  hand-rolled `interpretSpec` from the TCB.

Once both links are established, each `*SemEq` hypothesis in
`SolidityBridge.lean` becomes a provable lemma and the 46 bridge theorems
hold unconditionally against formally verified EVM semantics.

Machine-readable obligation status: [`config/semantic-bridge-obligations.json`](../config/semantic-bridge-obligations.json)

Lean-level obligation registry: `Morpho/Proofs/SemanticBridgeReadiness.lean`

## Planned Tracking Rules

1. Every bridge assumption must map to exactly one obligation ID.
2. Status values: `assumed | in_progress | discharged`.
3. CI must publish counts by status (`scripts/check_semantic_bridge_obligations.py`).
4. README safety claims must reference this table.

