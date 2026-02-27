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

## Planned Tracking Rules

1. Every bridge assumption must map to exactly one obligation ID.
2. Status values: `assumed | in_progress | proved | blocked`.
3. CI must publish counts by status.
4. README safety claims must reference this table.

