/-!
# Semantic Bridge Readiness

This module tracks the semantic equivalence obligations that must be discharged
to connect morpho-verity's invariant proofs to formally verified EVM semantics.

## Current State

`SolidityBridge.lean` proves 67 theorems that transfer Morpho invariants
(borrowLeSupply, alwaysCollateralized, irmMonotone, lltvMonotone) to any
Solidity implementation satisfying semantic equivalence hypotheses.

Each hypothesis (`supplySemEq`, `withdrawSemEq`, …) is a universally
quantified assumption — a function parameter, not a discharged proof.

## Discharge Path (verity#998)

The Verity semantic bridge will provide, for each function `f` in a
`verity_contract`:

```
EDSL.f.exec state = EVMYulLean(compile(spec)).exec state
```

This is a machine-checked composition of:
- **Layer 2** (CompilationModel → IR): proven in Verity
- **Layer 3** (IR → Yul/EVMYulLean): proven in Verity (1 keccak axiom)

To discharge morpho-verity's obligations, we need the chain:

```
Morpho.f args                          -- pure Lean model
  = MorphoViewSlice.f.exec state       -- EDSL macro output (currently stub for complex ops)
  = EVMYulLean(compile(spec)).exec     -- verified EVM semantics (verity bridge)
```

The first link (pure Lean ↔ EDSL) requires that `MorphoViewSlice` functions
have full (non-stub) implementations matching `Morpho.*`. The second link
is what the verity semantic bridge provides.

## Obligation Registry

Each entry below maps a `SolidityBridge.lean` hypothesis to its upstream
discharge dependency. Once both links above are established for an operation,
the corresponding `*SemEq` hypothesis becomes a provable lemma and the bridge
theorems hold unconditionally.
-/

namespace Morpho.Proofs.SemanticBridgeReadiness

/-- Status of a semantic bridge obligation. -/
inductive ObligationStatus where
  | assumed    : ObligationStatus
  | inProgress : ObligationStatus
  | discharged : ObligationStatus
  deriving Repr, DecidableEq

/-- A single semantic equivalence obligation connecting SolidityBridge.lean
    to the upstream verity semantic bridge. -/
structure SemanticBridgeObligation where
  /-- Identifier matching `config/semantic-bridge-obligations.json`. -/
  id : String
  /-- Name of the `*SemEq` definition in SolidityBridge.lean. -/
  hypothesis : String
  /-- Morpho operation name (e.g. "supply", "withdraw"). -/
  operation : String
  /-- Current status. -/
  status : ObligationStatus
  /-- Whether the operation has a full (non-stub) `verity_contract` macro
      implementation in MacroSlice.lean. When true, the macro-generated
      CompilationModel is ready for end-to-end semantic bridge composition
      once verity#998 lands. When false, macro migration must be completed
      before the obligation can be discharged. -/
  macroMigrated : Bool
  deriving Repr

/-- All 18 semantic equivalence obligations from SolidityBridge.lean.

    Each corresponds to one Morpho operation whose Solidity equivalence
    is hypothesized in bridge proofs and must eventually be discharged
    against EVMYulLean via the verity semantic bridge. -/
def obligations : List SemanticBridgeObligation := [
  { id := "OBL-SUPPLY-SEM-EQ"
    hypothesis := "supplySemEq"
    operation := "supply"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-WITHDRAW-SEM-EQ"
    hypothesis := "withdrawSemEq"
    operation := "withdraw"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-BORROW-SEM-EQ"
    hypothesis := "borrowSemEq"
    operation := "borrow"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-REPAY-SEM-EQ"
    hypothesis := "repaySemEq"
    operation := "repay"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-SUPPLY-COLLATERAL-SEM-EQ"
    hypothesis := "supplyCollateralSemEq"
    operation := "supplyCollateral"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-WITHDRAW-COLLATERAL-SEM-EQ"
    hypothesis := "withdrawCollateralSemEq"
    operation := "withdrawCollateral"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-LIQUIDATE-SEM-EQ"
    hypothesis := "liquidateSemEq"
    operation := "liquidate"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-ACCRUE-INTEREST-SEM-EQ"
    hypothesis := "accrueInterestSemEq"
    operation := "accrueInterest"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-ENABLE-IRM-SEM-EQ"
    hypothesis := "enableIrmSemEq"
    operation := "enableIrm"
    status := .assumed
    macroMigrated := true },
  { id := "OBL-ENABLE-LLTV-SEM-EQ"
    hypothesis := "enableLltvSemEq"
    operation := "enableLltv"
    status := .assumed
    macroMigrated := true },
  { id := "OBL-SET-AUTH-SEM-EQ"
    hypothesis := "setAuthorizationSemEq"
    operation := "setAuthorization"
    status := .assumed
    macroMigrated := true },
  { id := "OBL-SET-AUTH-SIG-SEM-EQ"
    hypothesis := "setAuthorizationWithSigSemEq"
    operation := "setAuthorizationWithSig"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-SET-OWNER-SEM-EQ"
    hypothesis := "setOwnerSemEq"
    operation := "setOwner"
    status := .assumed
    macroMigrated := true },
  { id := "OBL-SET-FEE-RECIPIENT-SEM-EQ"
    hypothesis := "setFeeRecipientSemEq"
    operation := "setFeeRecipient"
    status := .assumed
    macroMigrated := true },
  { id := "OBL-CREATE-MARKET-SEM-EQ"
    hypothesis := "createMarketSemEq"
    operation := "createMarket"
    status := .assumed
    macroMigrated := true },
  { id := "OBL-SET-FEE-SEM-EQ"
    hypothesis := "setFeeSemEq"
    operation := "setFee"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-ACCRUE-INTEREST-PUBLIC-SEM-EQ"
    hypothesis := "accrueInterestPublicSemEq"
    operation := "accrueInterestPublic"
    status := .assumed
    macroMigrated := false },
  { id := "OBL-FLASH-LOAN-SEM-EQ"
    hypothesis := "flashLoanSemEq"
    operation := "flashLoan"
    status := .assumed
    macroMigrated := false }
]

/-- All obligations are currently assumed (none discharged). -/
theorem all_assumed : obligations.all (fun o => o.status == .assumed) = true := by
  native_decide

/-- There are exactly 18 semantic equivalence obligations. -/
theorem obligation_count : obligations.length = 18 := by
  native_decide

/-- 6 of 18 operations have full (non-stub) macro implementations.
    These are ready for end-to-end semantic bridge composition once
    verity#998 lands: setOwner, setFeeRecipient, enableIrm, enableLltv,
    setAuthorization, createMarket. -/
theorem macro_migrated_count :
    (obligations.filter (fun o => o.macroMigrated)).length = 6 := by
  native_decide

/-- 12 operations still need macro migration before discharge. -/
theorem macro_pending_count :
    (obligations.filter (fun o => !o.macroMigrated)).length = 12 := by
  native_decide

end Morpho.Proofs.SemanticBridgeReadiness
