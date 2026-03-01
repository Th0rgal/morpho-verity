# Equivalence Obligations (Groundwork)

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document tracks the bridge assumptions that must become proved lemmas to support stronger Solidity-equivalence claims.

## Status

Groundwork only. Tracking format is proposed; current bridge assumptions are still assumptions.

## Scope

`Morpho/Proofs/SolidityBridge.lean` currently transfers invariants under semantic-equivalence hypotheses.
Each hypothesis must be tracked as a proof obligation with owner and status.

## Obligation Table

| Obligation ID | Bridge hypothesis | Operation | Macro migrated | Status |
|---------------|-------------------|-----------|:--------------:|--------|
| `OBL-SUPPLY-SEM-EQ` | `supplySemEq` | `supply` | | `assumed` |
| `OBL-WITHDRAW-SEM-EQ` | `withdrawSemEq` | `withdraw` | | `assumed` |
| `OBL-BORROW-SEM-EQ` | `borrowSemEq` | `borrow` | | `assumed` |
| `OBL-REPAY-SEM-EQ` | `repaySemEq` | `repay` | | `assumed` |
| `OBL-SUPPLY-COLLATERAL-SEM-EQ` | `supplyCollateralSemEq` | `supplyCollateral` | | `assumed` |
| `OBL-WITHDRAW-COLLATERAL-SEM-EQ` | `withdrawCollateralSemEq` | `withdrawCollateral` | | `assumed` |
| `OBL-LIQUIDATE-SEM-EQ` | `liquidateSemEq` | `liquidate` | | `assumed` |
| `OBL-ACCRUE-INTEREST-SEM-EQ` | `accrueInterestSemEq` | `accrueInterest` | | `assumed` |
| `OBL-ENABLE-IRM-SEM-EQ` | `enableIrmSemEq` | `enableIrm` | Y | `assumed` |
| `OBL-ENABLE-LLTV-SEM-EQ` | `enableLltvSemEq` | `enableLltv` | Y | `assumed` |
| `OBL-SET-AUTH-SEM-EQ` | `setAuthorizationSemEq` | `setAuthorization` | Y | `assumed` |
| `OBL-SET-AUTH-SIG-SEM-EQ` | `setAuthorizationWithSigSemEq` | `setAuthorizationWithSig` | | `assumed` |
| `OBL-SET-OWNER-SEM-EQ` | `setOwnerSemEq` | `setOwner` | Y | `assumed` |
| `OBL-SET-FEE-RECIPIENT-SEM-EQ` | `setFeeRecipientSemEq` | `setFeeRecipient` | Y | `assumed` |
| `OBL-CREATE-MARKET-SEM-EQ` | `createMarketSemEq` | `createMarket` | | `assumed` |
| `OBL-SET-FEE-SEM-EQ` | `setFeeSemEq` | `setFee` | | `assumed` |
| `OBL-ACCRUE-INTEREST-PUBLIC-SEM-EQ` | `accrueInterestPublicSemEq` | `accrueInterestPublic` | | `assumed` |
| `OBL-FLASH-LOAN-SEM-EQ` | `flashLoanSemEq` | `flashLoan` | | `assumed` |

**Macro migrated** = operation has a full (non-stub) `verity_contract` implementation in
`MacroSlice.lean` and is ready for end-to-end semantic bridge composition once verity#998
lands. 5/18 operations are macro-migrated; the remaining 13 are blocked on upstream macro
primitive support (internal calls, ERC20 module, callbacks, oracle calls, 2D struct access).

CI enforces macro migration status consistency: `scripts/check_semantic_bridge_obligations.py`
cross-references `macroMigrated` flags in config against stub detection in `MacroSlice.lean`.

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

## Spec Correspondence

For macro-migrated operations, `scripts/check_spec_correspondence.py` validates structural
correspondence between the macro-generated CompilationModel (`MacroSlice.lean`) and the
manual spec (`Spec.lean`):

- **Storage slots**: slot numbers must match between macro `storage` declarations and
  `Spec.lean` field definitions
- **Parameter count**: macro function parameters must match spec function parameters
- **Mutation count**: macro must not introduce spurious state mutations beyond if/else
  branching expansion (2x tolerance)
- **Stub detection**: macro-migrated operations must not be stubs

Known expected differences (not checked, handled by semantic bridge):
- **Events**: `Spec.lean` has `Stmt.emit` calls that MacroSlice omits (events don't affect state)
- **Stop**: `Spec.lean` ends functions with `Stmt.stop` (implicit in EDSL do-blocks)
- **Require expansion**: MacroSlice expands `requireOwner` into explicit `msgSender` + `require`

## Macro Migration Blockers

The 13 unmigrated operations depend on upstream verity macro capabilities:

| Blocker | Operations affected |
|---------|-------------------|
| Internal function calls (`Stmt.internalCall`) | supply, withdraw, borrow, repay, liquidate, setFee, accrueInterest, accrueInterestPublic |
| ERC20 module (`ERC20.safeTransfer/From`) | supply, withdraw, borrow, repay, supplyCollateral, withdrawCollateral, liquidate, flashLoan |
| External callbacks (`Callbacks.callback`) | supply, supplyCollateral, repay, liquidate, flashLoan |
| External contract calls (`Calls.withReturn`) | accrueInterest, accrueInterestPublic, withdrawCollateral, borrow, liquidate |
| 2D struct mapping (`structMember2`) | supply, withdraw, borrow, repay, supplyCollateral, withdrawCollateral, liquidate |
| Memory management (`mstore/mload`) | setAuthorizationWithSig, liquidate |
| Precompile access (`ecrecover`) | setAuthorizationWithSig |
| Tuple destructuring | createMarket, setFee |

## Planned Tracking Rules

1. Every bridge assumption must map to exactly one obligation ID.
2. Status values: `assumed | in_progress | discharged`.
3. CI must publish counts by status (`scripts/check_semantic_bridge_obligations.py`).
4. README safety claims must reference this table.

