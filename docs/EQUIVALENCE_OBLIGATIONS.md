# Equivalence Obligations (Groundwork)

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document tracks the bridge assumptions that must become proved lemmas to support stronger Solidity-equivalence claims.

## Status

6/18 obligations have Link 1 (stable `Morpho.*` wrapper API ↔ EDSL) proven: setOwner, setFeeRecipient,
enableIrm, enableLltv, setAuthorization, flashLoan. The proofs are in
`Morpho/Proofs/SemanticBridgeDischarge.lean`.

Links 2+3 (EDSL → IR → Yul compilation correctness) are **delegated to Verity's
compiler framework**. The `verity_contract` macro generates code exclusively from
supported patterns, so `SupportedStmtList`/`SupportedSpec` witnesses should be
constructive — produced by the macro, not proven manually per-contract. Manual
witness proofs have been removed from this repository.

## Scope

`Morpho/Proofs/SolidityBridge.lean` currently transfers invariants under semantic-equivalence hypotheses.
Each hypothesis must be tracked as a proof obligation with owner and status.

## Obligation Table

| Obligation ID | Bridge hypothesis | Operation | Macro migrated | Status |
|---------------|-------------------|-----------|:--------------:|--------|
| `OBL-SUPPLY-SEM-EQ` | `supplySemEq` | `supply` | Y | `assumed` |
| `OBL-WITHDRAW-SEM-EQ` | `withdrawSemEq` | `withdraw` | Y | `assumed` |
| `OBL-BORROW-SEM-EQ` | `borrowSemEq` | `borrow` | Y | `assumed` |
| `OBL-REPAY-SEM-EQ` | `repaySemEq` | `repay` | Y | `assumed` |
| `OBL-SUPPLY-COLLATERAL-SEM-EQ` | `supplyCollateralSemEq` | `supplyCollateral` | Y | `assumed` |
| `OBL-WITHDRAW-COLLATERAL-SEM-EQ` | `withdrawCollateralSemEq` | `withdrawCollateral` | Y | `assumed` |
| `OBL-LIQUIDATE-SEM-EQ` | `liquidateSemEq` | `liquidate` | Y | `assumed` |
| `OBL-ACCRUE-INTEREST-SEM-EQ` | `accrueInterestSemEq` | `accrueInterest` | Y | `assumed` |
| `OBL-ENABLE-IRM-SEM-EQ` | `enableIrmSemEq` | `enableIrm` | Y | `link1_proven` |
| `OBL-ENABLE-LLTV-SEM-EQ` | `enableLltvSemEq` | `enableLltv` | Y | `link1_proven` |
| `OBL-SET-AUTH-SEM-EQ` | `setAuthorizationSemEq` | `setAuthorization` | Y | `link1_proven` |
| `OBL-SET-AUTH-SIG-SEM-EQ` | `setAuthorizationWithSigSemEq` | `setAuthorizationWithSig` | Y | `assumed` |
| `OBL-SET-OWNER-SEM-EQ` | `setOwnerSemEq` | `setOwner` | Y | `link1_proven` |
| `OBL-SET-FEE-RECIPIENT-SEM-EQ` | `setFeeRecipientSemEq` | `setFeeRecipient` | Y | `link1_proven` |
| `OBL-CREATE-MARKET-SEM-EQ` | `createMarketSemEq` | `createMarket` | Y | `assumed` |
| `OBL-SET-FEE-SEM-EQ` | `setFeeSemEq` | `setFee` | Y | `assumed` |
| `OBL-ACCRUE-INTEREST-PUBLIC-SEM-EQ` | `accrueInterestPublicSemEq` | `accrueInterestPublic` | Y | `assumed` |
| `OBL-FLASH-LOAN-SEM-EQ` | `flashLoanSemEq` | `flashLoan` | Y | `link1_proven` |

**Macro migrated** = operation has a full (non-stub) `verity_contract` implementation in
`MacroSlice.lean`, which is the current macro-generated contract surface. 18/18 operations are
macro-migrated (`accrueInterestPublic` maps to the fully migrated `accrueInterest` function).
CI enforces macro migration status consistency: `scripts/check_semantic_bridge_obligations.py`
cross-references `macroMigrated` flags in config against stub detection in `MacroSlice.lean`.
`scripts/check_equivalence_obligations_doc.py` now also fail-closes the top-level Link 1 /
macro-migration status summary and the markdown issue-cluster table below against the tracked
obligation inventory, so the repo-facing roadmap in this document cannot silently drift from the
machine-checked migration state.
`scripts/check_macro_migration_blockers_doc.py` also now fail-closes the remaining macro blocker
table below against the false `macroMigrated` obligation inventory, so the aggregate blocker
families and counts cannot silently drift from the tracked migration backlog.
`scripts/check_semantic_bridge_readiness_sync.py` also now fail-closes the duplicated Lean
readiness registry in `Morpho/Proofs/SemanticBridgeReadiness.lean` against the JSON tracker,
so status and `macroMigrated` flips cannot silently diverge across proof-facing surfaces.
`scripts/check_semantic_bridge_readiness_summary.py` also fail-closes the proof-facing summary
counts and Link 1 operation list in that Lean file, so future `discharged` flips cannot leave
the readiness narrative claiming fewer proved operations than the tracker actually records.
`scripts/check_verity_pin_provenance.py` also now fail-closes the current-pin macro/frontend
divergence section in `docs/VERITY_PIN.md` against `config/verity-pin-provenance.json`, including
the exact blocker-family, issue-cluster, and relevant-file bullet lists, so that surface cannot
silently drift away from the tracked migration roadmap.

### Blocker cluster summary

| Cluster | Operations | Blocker families | Coverage counts |
|-------|------------|------------------|-----------------|

## Semantic Bridge Discharge Path

The obligations above will be discharged via the Verity hybrid canonical-semantics migration
([verity#1060](https://github.com/Th0rgal/verity/issues/1060),
[verity#1065](https://github.com/Th0rgal/verity/pull/1065)).

The discharge requires a two-layer correspondence for each operation:

```
Morpho.f args                        -- stable wrapper surface (this repo)
  = MorphoViewSlice.f.exec state     -- EDSL macro output (verity_contract)
  = EVMYulLean(compile(spec)).exec   -- verified EVM semantics (Verity compiler, trusted)
```

- **Link 1** (stable wrapper API ↔ EDSL): requires that `MorphoViewSlice`
  functions in `MacroSlice.lean` have full implementations matching
  `Morpho.*`, which in turn aliases `Morpho.Specs.ContractSemantics` for the
  migrated operations. All 18/18 operations now have full macro implementations;
  the remaining Link 1 gaps are proof-level (semantic-equivalence theorems
  for complex operations like supply, borrow, liquidate, etc.).

- **Links 2+3** (EDSL ↔ EVMYulLean): delegated to Verity's compiler
  framework. The compiler should produce `SupportedStmtList` witnesses
  constructively from the `verity_contract` macro output.

Once both links are established, each `*SemEq` hypothesis in
`SolidityBridge.lean` becomes a provable lemma and the 67 bridge theorems
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

All 18/18 operations are now macro-migrated. `accrueInterestPublic` maps to the
fully migrated `accrueInterest` function in MacroSlice (body inlined).

**Resolved at the current pin** (`4ebe4931`): `setStructMember`/`structMember`
statement/expression primitives, `getMappingUint`/`setMappingUint` explicit
translators, `Bytes32`/`Bool` type support, linked externals, direct ERC20 helper syntax,
tuple params, `MappingStruct`/`MappingStruct2`, `internalCall`, `mstore`/`mload`,
macro-backed `ecrecover`, events via `emit`, `safeTransfer`/`safeTransferFrom`,
`ecmDo` for callback invocations, and `uint128` overflow guards.

**Remaining blockers**:

| Blocker | Operations affected | Count |
|---------|-------------------|:-----:|

## Primitive Coverage & Discharge Readiness

For macro-migrated operations, `scripts/check_primitive_coverage.py` analyzes which EDSL
primitives each function uses and cross-references against proven `PrimitiveBridge` lemmas
in the upstream verity roadmap branch.

### Current status (verity `roadmap/1060-hybrid-migration` branch, 2026-03-01)

**Proven PrimitiveBridge lemmas** (EDSL ↔ compiled Yul): `getStorage`, `setStorage`,
`getStorageAddr`, `setStorageAddr`, `require` (eq/neq/lt/gt), `msgSender`, `if_then_else`,
`uint256.add/sub/mul/div/mod`, `uint256.lt/gt/eq`, `calldataloadWord` (encoding/decoding),
`safeAdd`/`safeSub` (overflow/underflow), `getMapping_unfold`/`setMapping_unfold`

**Proven semantic bridge theorems** (zero sorry, as tracked in verity roadmap snapshots):
- SimpleStorage: `store`, `retrieve` — Uint256-typed storage
- Counter: `increment`, `decrement`, `getCount` — arithmetic + Uint256 storage
- Owned: `getOwner`, `transferOwnership` — **Address-typed storage + access control**
- SafeCounter: `increment`, `decrement`, `getCount` — checked arithmetic, revert cases
- OwnedCounter: mixed-type multi-slot encoding with access control

The `owned_transferOwnership_semantic_bridge` theorem is the direct template for
morpho-verity's `setOwner` discharge. It proves EDSL ≡ compiled IR for an ownership
transfer pattern using `encodeStorageAddr`, direct `simp` unfolding, and
`Nat.and_two_pow_sub_one_eq_mod` for address masking.

The `ownedCounter` proofs demonstrate mixed Address+Uint256 slots via `encodeMixedStorage`
and mapping operations via `getMapping_unfold`/`setMapping_unfold`, which are directly
applicable to morpho-verity's enableIrm/enableLltv/setAuthorization operations.

**Proven MappingAutomation lemmas** (EDSL-level, zero sorry): `getMapping`,
`setMapping` (read-after-write, non-interference), `getMappingUint`, `setMappingUint`,
`getMapping2`, `setMapping2` — complete read/write/cross-slot preservation.
These operate at the `ContractState` level; the EDSL-to-Yul bridge for mapping
operations (keccak-based slot computation) is not yet in PrimitiveBridge.

| Operation | Primitives used | Link 1 | Links 2+3 |
|-----------|----------------|:------:|:---------:|
| `setOwner` | getStorageAddr, setStorageAddr, msgSender, require | **PROVEN** | Verity (trusted) |
| `setFeeRecipient` | getStorageAddr (×2), setStorageAddr, msgSender, require | **PROVEN** | Verity (trusted) |
| `enableIrm` | getMapping, setMapping, getStorageAddr, msgSender, require | **PROVEN** | Verity (trusted) |
| `enableLltv` | getMappingUint, setMappingUint, getStorageAddr, msgSender, require | **PROVEN** | Verity (trusted) |
| `setAuthorization` | getMapping2, setMapping2, if_then_else, msgSender, require | **PROVEN** | Verity (trusted) |
| `flashLoan` | msgSender, require, emit, safeTransfer, safeTransferFrom | **PROVEN** | Verity (trusted) |
| `createMarket` | getMapping, getMappingUint, structMember, setStructMember, externalCall, blockTimestamp | pending | Verity (trusted) |

**Summary**: 18/18 operations are macro-migrated, and 6 of those have Link 1
(stable wrapper API ↔ EDSL) fully proven. Links 2+3 (compilation correctness)
are delegated to Verity's compiler framework.

### Discharge proof structure

`Morpho/Proofs/SemanticBridgeDischarge.lean` contains actual proofs for the
first link of the discharge chain: **Pure Lean ↔ EDSL equivalence**.

The discharge has two links per obligation:
1. **Link 1** (this repo): `Morpho.f ↔ MorphoViewSlice.f` — proven for setOwner, setFeeRecipient, enableIrm, enableLltv, setAuthorization, flashLoan
2. **Links 2+3** (Verity): `EDSL ↔ EVMYulLean(Yul)` — delegated to Verity's compiler framework (trusted)

**Link 1 proof pattern** (for the 6 proven operations):
1. Define `encodeMorphoState : MorphoState → ContractState` matching MacroSlice storage
2. Run EDSL function on encoded state, decode result to `Option MorphoState`
3. Unfold EDSL monadic chain (`bind`, `msgSender`, `getStorageAddr`, `require`, etc.)
4. `split <;> simp_all` closes all cases after `beq_iff_eq`/`bne_iff_ne` normalization

### Discharge sequence (current pin: `4ebe4931`)

1. **Link 1 proven (6 ops)**: `setOwner`, `setFeeRecipient`, `enableIrm`, `enableLltv`,
   `setAuthorization`, `flashLoan` — Link 1 proven in `SemanticBridgeDischarge.lean`.
2. **After Link 1 proofs for remaining operations**: 12 operations — requires
   semantic-equivalence theorems connecting the EDSL implementations to the pure Lean models.
3. **`accrueInterestPublic` wrapper**: resolved — maps to the fully migrated `accrueInterest`.

Machine-readable primitive coverage: `scripts/check_primitive_coverage.py --json-out`

## Planned Tracking Rules

1. Every bridge assumption must map to exactly one obligation ID.
2. Status values: `assumed | in_progress | discharged`.
3. CI must publish counts by status (`scripts/check_semantic_bridge_obligations.py`).
4. README safety claims must reference this table.
