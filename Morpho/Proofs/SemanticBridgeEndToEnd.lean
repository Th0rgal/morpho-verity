import Morpho.Proofs.SemanticBridgeDischarge
import Morpho.Proofs.Invariants

/-!
# Semantic Bridge End-to-End: Direct Link 1 Composition + Links 2+3 Gap

This module demonstrates direct composition of Link 1 proofs with invariant
theorems, and documents the precise gap for Links 2+3 (EDSL ↔ compiled IR ↔ Yul).

## What this proves (no sorry)

For setOwner, we compose:
- Link 1 (SemanticBridgeDischarge): `edslSetOwner = Morpho.setOwner`
- Pure invariant (Invariants): `Morpho.setOwner` preserves `borrowLeSupply` etc.

Result: the EDSL `setOwner` preserves all Morpho invariants, proven by direct
composition rather than through the parametric SolidityBridge layer.

## Links 2+3 status (post-verity#1065 pin bump)

The verity pin has been bumped from `dccb984` to `fc661db2` (post-`verity#1065`).
The new verity provides:
- `TypedIRCompiler.lean` + `TypedIRCompilerCorrectness.lean`: generic typed-IR
  compilation-correctness theorem — if a function body is a `SupportedStmtList`,
  `execCompiledSupportedStmtFragments = execSourceSupportedStmtFragments` with no
  per-function proof needed.
- The old `SpecInterpreter` + manual `SpecCorrectness` proofs have been removed;
  Links 2+3 are now obtained by proving function bodies are `SupportedStmtList`
  (typically `by decide` or `by native_decide`).

Next steps for Links 2+3:
1. Prove each `verity_contract` function body is a `SupportedStmtList`
2. The generic theorem then gives compilation correctness for free
3. Compose with Link 1 for the full chain: Pure Lean ↔ EDSL ↔ IR ↔ Yul
-/

namespace Morpho.Proofs.SemanticBridgeEndToEnd

open Verity
open Morpho.Types
open Morpho.Proofs.SemanticBridgeDischarge
open Morpho.Proofs.Invariants
open Morpho.Specs.Invariants

/-! ## Direct Link 1 + Invariants composition

These theorems compose Link 1 (`edslSetOwner = Morpho.setOwner`) directly
with the pure invariant theorems, bypassing the parametric SolidityBridge layer.
This is the most direct proof that the EDSL implementation preserves invariants.
-/

/-- The EDSL `setOwner` preserves borrow-le-supply, by direct composition
    of Link 1 with the pure model invariant. -/
theorem edsl_setOwner_borrowLeSupply
    (s : MorphoState) (newOwner : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslSetOwner s newOwner = some s') :
    borrowLeSupply s' id := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_borrowLeSupply s newOwner id h_solvent h_morpho

/-- The EDSL `setOwner` preserves always-collateralized. -/
theorem edsl_setOwner_alwaysCollateralized
    (s : MorphoState) (newOwner : Address) (id : Id) (user : Address) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : edslSetOwner s newOwner = some s') :
    alwaysCollateralized s' id user := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_alwaysCollateralized s newOwner id user h_collat h_morpho

/-- The EDSL `setOwner` preserves IRM monotonicity. -/
theorem edsl_setOwner_irmMonotone
    (s : MorphoState) (newOwner irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslSetOwner s newOwner = some s') :
    s'.isIrmEnabled irm := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_irmMonotone s newOwner irm h_enabled h_morpho

/-- The EDSL `setOwner` preserves LLTV monotonicity. -/
theorem edsl_setOwner_lltvMonotone
    (s : MorphoState) (newOwner : Address) (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : edslSetOwner s newOwner = some s') :
    s'.isLltvEnabled lltv := by
  have h_morpho : Morpho.setOwner s newOwner = some s' := by
    rw [← setOwner_link1]; exact h_ok
  exact setOwner_preserves_lltvMonotone s newOwner lltv h_enabled h_morpho

/-! ## enableIrm: Direct composition -/

theorem edsl_enableIrm_borrowLeSupply
    (s : MorphoState) (irm : Address) (id : Id) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : edslEnableIrm s irm = some s') :
    borrowLeSupply s' id := by
  have h_morpho : Morpho.enableIrm s irm = some s' := by
    rw [← enableIrm_link1]; exact h_ok
  exact enableIrm_preserves_borrowLeSupply s irm id h_solvent h_morpho

theorem edsl_enableIrm_irmMonotone
    (s : MorphoState) (irmCall irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : edslEnableIrm s irmCall = some s') :
    s'.isIrmEnabled irm := by
  have h_morpho : Morpho.enableIrm s irmCall = some s' := by
    rw [← enableIrm_link1]; exact h_ok
  exact enableIrm_monotone s irmCall irm h_morpho h_enabled

/-! ## Links 2+3: SupportedStmtFragment gap (verity#1066)

The typed-IR framework requires proving each function body is a `SupportedStmtList`.
However, the existing `SupportedStmtFragment` constructors do not cover the
`letVar`-expanded patterns that the `verity_contract` macro generates.

### The structural mismatch

The macro generates bodies like:
```
letVar "sender" Expr.caller
letVar "currentOwner" (Expr.storage "ownerSlot")
require (localVar "sender" == localVar "currentOwner") "not owner"
...
```

But the existing guard clause framework (`RequireLiteralGuardFamilyClause`) only
supports `Expr.literal` in require guards. There are no constructors for the
`letVar + require(localVar == localVar)` patterns.

### Upstream issue: verity#1066

New `SupportedStmtFragment` constructors are needed for:
1. **setOwner**: letCaller + letStorageAddr + reqEq + reqNeq + setStorageAddr(param) + stop
2. **setFeeRecipient**: same but writes to different field
3. **enableIrm**: owner check + letMapping + reqEqLiteral + setMapping(param, literal) + stop
4. **enableLltv**: owner check + letMappingUint + reqEqLiteral + reqLt + setMappingUint + stop
5. **setAuthorization**: letCaller + letMapping2 + ite(param, [req+setMapping2], [req+setMapping2]) + stop

Tracked in https://github.com/Th0rgal/verity/issues/1066.

### createMarket:

Still BLOCKED: `getMappingWord`/`setMappingWord` are stubs (`pure 0`/`pure ()`) in
`MacroSlice.lean`. The EDSL implementation does not actually read/write market
struct data. This must be resolved before Link 1 can be proven for createMarket.
-/

end Morpho.Proofs.SemanticBridgeEndToEnd
