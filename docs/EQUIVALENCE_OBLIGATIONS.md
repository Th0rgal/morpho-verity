# Equivalence Obligations (Groundwork)

Issue: [#25](https://github.com/Th0rgal/morpho-verity/issues/25)

This document tracks the bridge assumptions that must become proved lemmas to support stronger Solidity-equivalence claims.

## Status

6/18 obligations have Link 1 (stable `Morpho.*` wrapper API ↔ EDSL) proven: setOwner, setFeeRecipient,
enableIrm, enableLltv, setAuthorization, flashLoan. The proofs are in
`Morpho/Proofs/SemanticBridgeDischarge.lean`.

5 of the 6 also have Link 2 (EDSL ↔ SupportedStmtList witness) proven in
`Morpho/Proofs/CompilationCorrectness.lean` (setOwner, setFeeRecipient,
enableIrm, enableLltv, setAuthorization). `flashLoan` is now in the same
state as a Link 1-proven operation, but still lacks Link 2 because its
event-emission body needs a `SupportedStmtList` witness for a `rawLog` tail
whose topics depend on `caller` and `token`, not just literals. Link 3 (CompilationModel ↔ EVMYulLean) depends on
upstream verity infrastructure.

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
| `OBL-ENABLE-IRM-SEM-EQ` | `enableIrmSemEq` | `enableIrm` | Y | `link1_proven` |
| `OBL-ENABLE-LLTV-SEM-EQ` | `enableLltvSemEq` | `enableLltv` | Y | `link1_proven` |
| `OBL-SET-AUTH-SEM-EQ` | `setAuthorizationSemEq` | `setAuthorization` | Y | `link1_proven` |
| `OBL-SET-AUTH-SIG-SEM-EQ` | `setAuthorizationWithSigSemEq` | `setAuthorizationWithSig` | Y | `assumed` |
| `OBL-SET-OWNER-SEM-EQ` | `setOwnerSemEq` | `setOwner` | Y | `link1_proven` |
| `OBL-SET-FEE-RECIPIENT-SEM-EQ` | `setFeeRecipientSemEq` | `setFeeRecipient` | Y | `link1_proven` |
| `OBL-CREATE-MARKET-SEM-EQ` | `createMarketSemEq` | `createMarket` | Y | `assumed` |
| `OBL-SET-FEE-SEM-EQ` | `setFeeSemEq` | `setFee` | | `assumed` |
| `OBL-ACCRUE-INTEREST-PUBLIC-SEM-EQ` | `accrueInterestPublicSemEq` | `accrueInterestPublic` | | `assumed` |
| `OBL-FLASH-LOAN-SEM-EQ` | `flashLoanSemEq` | `flashLoan` | Y | `link1_proven` |

**Macro migrated** = operation has a full (non-stub) `verity_contract` implementation in
`MacroSlice.lean`, which is the current macro-generated contract surface. 8/18 operations are
macro-migrated; the remaining 10 are blocked on upstream macro
primitive support (internal calls, ERC20 module, callbacks, oracle calls, 2D struct access).
For the 7 core/collateral flow stubs tracked under blocker-cluster labels `#123`/`#124`,
`config/semantic-bridge-obligations.json` now also
tracks machine-readable `macroSurfaceBlockers` arrays, and
`scripts/check_macro_migration_blockers.py` fail-closes if those per-operation blocker families
drift from `Morpho/Compiler/Spec.lean`.
The same regression suite also compile-checks minimal `verity_contract` repros for the current
core-flow frontend gaps: `Calls.withReturn`, internal `call`, `Callbacks.callback`,
`ERC20.safeTransfer`/`ERC20.safeTransferFrom`, `structMember2` read/write, and direct
`mstore`/`mload` still fail at the pinned verity revision.
For the collateral/liquidation cluster, the same suite separately pins the full current
frontend blocker surface too: `Calls.withReturn`, internal `call`,
`Callbacks.callback`, `ERC20.safeTransfer`/`ERC20.safeTransferFrom`,
`structMember2` read/write, and direct `mstore`/`mload` all still fail for the current
`supplyCollateral`, `withdrawCollateral`, and `liquidate` spec shapes.
`scripts/check_macro_blocker_regression_coverage.py` now fail-closes the remaining gap
between that compile-checked suite and the tracked blocker clusters by requiring every current
`#123`/`#124` `macroSurfaceBlockers` family to have at least one named regression case.
`scripts/check_issue_blocker_clusters.py` now also derives fail-closed issue-cluster summaries
from per-obligation `issue` tags in the obligation tracker so the remaining migration clusters
cannot drift from the actual per-operation blocker inventory.
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
| `#123` | `supply`, `withdraw`, `borrow`, `repay` | `callbacks`, `erc20`, `externalWithReturn`, `internalCall`, `memoryOps`, `structMember2` | callbacks×2, erc20×4, externalWithReturn×1, internalCall×4, memoryOps×4, structMember2×4 |
| `#124` | `supplyCollateral`, `withdrawCollateral`, `liquidate` | `callbacks`, `erc20`, `externalWithReturn`, `internalCall`, `memoryOps`, `structMember2` | callbacks×2, erc20×3, externalWithReturn×2, internalCall×2, memoryOps×3, structMember2×3 |

CI enforces macro migration status consistency: `scripts/check_semantic_bridge_obligations.py`
cross-references `macroMigrated` flags in config against stub detection in `MacroSlice.lean`.
`createMarket` is now macro-migrated at the current pin; the remaining gap there is the
semantic-equivalence theorem back to the handwritten `Morpho.createMarket` model.

## Semantic Bridge Discharge Path

The obligations above will be discharged via the Verity hybrid canonical-semantics migration
([verity#1060](https://github.com/Th0rgal/verity/issues/1060),
[verity#1065](https://github.com/Th0rgal/verity/pull/1065)).

The discharge requires a three-layer correspondence for each operation:

```
Morpho.f args                        -- stable wrapper surface (this repo)
  = MorphoViewSlice.f.exec state     -- EDSL macro output (verity_contract)
  = EVMYulLean(compile(spec)).exec   -- verified EVM semantics (verity bridge)
```

- **Link 1** (stable wrapper API ↔ EDSL): requires that `MorphoViewSlice`
  functions in `MacroSlice.lean` have full implementations matching
  `Morpho.*`, which in turn aliases `Morpho.Specs.ContractSemantics` for the
  migrated operations. Currently, simple view functions are fully implemented;
  complex operations (supply, borrow, liquidate, etc.) are stub/noop — these
  must be completed as macro primitive support grows in verity.

- **Link 2** (EDSL ↔ EVMYulLean): provided upstream for the supported fragment
  via verity's typed-IR / canonical-semantics bridge. This eliminates the
  hand-rolled `interpretSpec` from the TCB where the macro frontend can lower
  the contract successfully.

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

The 10 unmigrated operations depend on upstream verity macro capabilities.

**Resolved/usable at the current pin**: `setStructMember`/`structMember`
statement/expression primitives, `getMappingUint`/`setMappingUint` explicit
translators, and `Bytes32`/`Bool` type support.

At `7b7c9193`, the repo can now use linked externals, direct ERC20 helper syntax,
tuple params in executable `verity_contract` definitions, `MappingStruct`/`MappingStruct2`,
`internalCall`, `mstore`/`mload`, and macro-backed `ecrecover`. The remaining gaps are now narrower and mostly concern
proof coverage or missing Morpho-side abstractions, not raw macro elaboration.

**Remaining blockers**:

| Blocker | Operations affected | Count |
|---------|-------------------|:-----:|
| Internal function calls (`Stmt.internalCall`) | `accrueInterest`, `accrueInterestPublic`, `borrow`, `liquidate`, `repay`, `setFee`, `supply`, `withdraw`, `withdrawCollateral` | 9 |
| ERC20 module (`ERC20.safeTransfer/From`) | `borrow`, `liquidate`, `repay`, `supply`, `supplyCollateral`, `withdraw`, `withdrawCollateral` | 7 |
| 2D struct mapping read/write (`structMember2`) | `borrow`, `liquidate`, `repay`, `supply`, `supplyCollateral`, `withdraw`, `withdrawCollateral` | 7 |
| External callbacks (`Callbacks.callback`) | `liquidate`, `repay`, `supply`, `supplyCollateral` | 4 |
| External contract calls (`Calls.withReturn`) | `accrueInterest`, `accrueInterestPublic`, `borrow`, `liquidate`, `withdrawCollateral` | 5 |
| `.mappingStruct` storage field type declarations | `accrueInterest`, `accrueInterestPublic`, `setFee` | 3 |
| Memory management (`mstore/mload`) | `borrow`, `liquidate`, `repay`, `supply`, `supplyCollateral`, `withdraw`, `withdrawCollateral` | 7 |
**Note on createMarket**: The macro body now uses the direct struct-storage path for
`market` and `idToMarketParams`. What is still missing is the theorem that identifies
that macro-backed adapter with the handwritten `Morpho.createMarket` state model used
by the current bridge obligations.

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

| Operation | Primitives used | Link 1 | Link 2 (CompilationCorrectness) | Link 3 |
|-----------|----------------|:------:|:-------------------------------:|--------|
| `setOwner` | getStorageAddr, setStorageAddr, msgSender, require | **PROVEN** | **PROVEN** | available at verity pin 7b7c9193 |
| `setFeeRecipient` | getStorageAddr (×2), setStorageAddr, msgSender, require | **PROVEN** | **PROVEN** | available at verity pin 7b7c9193 |
| `enableIrm` | getMapping, setMapping, getStorageAddr, msgSender, require | **PROVEN** | **PROVEN** | available at verity pin 7b7c9193 |
| `enableLltv` | getMappingUint, setMappingUint, getStorageAddr, msgSender, require | **PROVEN** | **PROVEN** | available at verity pin 7b7c9193 |
| `setAuthorization` | getMapping2, setMapping2, if_then_else, msgSender, require | **PROVEN** | **PROVEN** | available at verity pin 7b7c9193 |
| `flashLoan` | msgSender, require, mstore, rawLog | **PROVEN** | pending | dynamic-topic rawLog witness + external I/O bridge coverage |
| `createMarket` | getMapping, getMappingUint, structMember, setStructMember, externalCall, blockTimestamp | pending | pending | semantic bridge + SupportedStmtList witness for struct-storage write path |

**Summary**: 8 operations are now macro-migrated, and 6 of those have Link 1
(stable wrapper API ↔ EDSL) fully proven.
The 5 admin operations also now have Link 2 (EDSL ↔ SupportedStmtList) proven in
`Morpho/Proofs/CompilationCorrectness.lean`, including `setFeeRecipient` via
the upstream two-storage-address witness added in verity. `flashLoan` remains
blocked at Link 2 on the dynamic-topic `rawLog` event path.
`createMarket` is macro-migrated but its semantic-equivalence theorem is not yet discharged.

### Discharge proof structure

`Morpho/Proofs/SemanticBridgeDischarge.lean` contains actual proofs for the
first link of the discharge chain: **Pure Lean ↔ EDSL equivalence**.

The discharge has three links per obligation:
1. **Link 1** (this repo): `Morpho.f ↔ MorphoViewSlice.f` — proven for setOwner, setFeeRecipient, enableIrm, enableLltv, setAuthorization, flashLoan
2. **Link 2** (this repo, current pin): `EDSL ↔ SupportedStmtList witness` — proven for setOwner, setFeeRecipient, enableIrm, enableLltv, setAuthorization in `Morpho/Proofs/CompilationCorrectness.lean`
3. **Link 3** (verity): `CompilationModel ↔ EVMYulLean(Yul)` — EndToEnd theorem

At verity pin `7b7c9193`, Link 2 is tracked on the typed-IR semantic bridge path
with concrete upstream witness theorems for Morpho admin patterns.

**Link 1 proof pattern** (for the 5 admin operations):
1. Define `encodeMorphoState : MorphoState → ContractState` matching MacroSlice storage
2. Run EDSL function on encoded state, decode result to `Option MorphoState`
3. Unfold EDSL monadic chain (`bind`, `msgSender`, `getStorageAddr`, `require`, etc.)
4. `split <;> simp_all` closes all cases after `beq_iff_eq`/`bne_iff_ne` normalization

**Link 2 proof pattern** (for 5 proven operations in `Morpho/Proofs/CompilationCorrectness.lean`):
1. Define `morphoFields : List Field` matching the `verity_contract MorphoViewSlice` storage layout
2. State theorem: `SupportedStmtList morphoFields <function_body_stmts>`
3. Construct witness using the appropriate `SupportedStmtFragment` constructor
4. Close obligations via `native_decide` (field resolution) and `decide` (literal checks)
5. `setFeeRecipient` now uses verity's two-storage-address `SupportedStmtFragment` constructor for the owner/auth + fee-recipient inequality pattern

### Discharge sequence (current pin: `7b7c9193`)

1. **Links 1+2 proven (5 ops), Link 3 via verity EndToEnd composition**: `setOwner`,
   `setFeeRecipient`, `enableIrm`, `enableLltv`, `setAuthorization` — Link 1 proven in
   `SemanticBridgeDischarge.lean`, Link 2 proven in `CompilationCorrectness.lean`.
2. **After semantic-bridge discharge of the new macro body**: `createMarket` — implementation
   restored at the current pin; still needs the theorem tying the macro-backed adapter to
   the handwritten `Morpho.createMarket` state model
3. **After remaining macro expansion**: 11 operations — requires internal calls, ERC20,
   callbacks, external contract calls

Machine-readable primitive coverage: `scripts/check_primitive_coverage.py --json-out`

## Planned Tracking Rules

1. Every bridge assumption must map to exactly one obligation ID.
2. Status values: `assumed | in_progress | discharged`.
3. CI must publish counts by status (`scripts/check_semantic_bridge_obligations.py`).
4. README safety claims must reference this table.
