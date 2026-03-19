import Compiler.CompilationModel
import Compiler.Selector
import Morpho.Compiler.MacroSlice

namespace Morpho.Compiler.Generated

open Compiler.CompilationModel

/--
Canonical compiler input boundary for Morpho.

Backed by the macro-generated `verity_contract` artifact so production compile
path no longer depends on manual `Spec.morphoSpec` authoring.
-/

private def morphoEvents : List EventDef := [
  { name := "SetOwner",             params := [⟨"newOwner",    .address, .unindexed⟩] },
  { name := "SetFeeRecipient",      params := [⟨"newFeeRecipient", .address, .unindexed⟩] },
  { name := "EnableIrm",            params := [⟨"irm",         .address, .unindexed⟩] },
  { name := "EnableLltv",           params := [⟨"lltv",        .uint256, .unindexed⟩] },
  { name := "CreateMarket",         params := [⟨"id",          .uint256, .unindexed⟩] },
  { name := "SetFee",               params := [⟨"id",          .uint256, .unindexed⟩, ⟨"newFee", .uint256, .unindexed⟩] },
  { name := "SupplyCollateral",     params := [⟨"id",          .uint256, .unindexed⟩, ⟨"sender", .address, .unindexed⟩, ⟨"onBehalf", .address, .unindexed⟩, ⟨"assets", .uint256, .unindexed⟩] },
  { name := "WithdrawCollateral",   params := [⟨"id",          .uint256, .unindexed⟩, ⟨"sender", .address, .unindexed⟩, ⟨"onBehalf", .address, .unindexed⟩, ⟨"receiver", .address, .unindexed⟩, ⟨"assets", .uint256, .unindexed⟩] },
  { name := "Supply",               params := [⟨"id",          .uint256, .unindexed⟩, ⟨"sender", .address, .unindexed⟩, ⟨"onBehalf", .address, .unindexed⟩, ⟨"assets", .uint256, .unindexed⟩, ⟨"shares", .uint256, .unindexed⟩] },
  { name := "Withdraw",             params := [⟨"id",          .uint256, .unindexed⟩, ⟨"sender", .address, .unindexed⟩, ⟨"onBehalf", .address, .unindexed⟩, ⟨"receiver", .address, .unindexed⟩, ⟨"assets", .uint256, .unindexed⟩, ⟨"shares", .uint256, .unindexed⟩] },
  { name := "Borrow",               params := [⟨"id",          .uint256, .unindexed⟩, ⟨"sender", .address, .unindexed⟩, ⟨"onBehalf", .address, .unindexed⟩, ⟨"receiver", .address, .unindexed⟩, ⟨"assets", .uint256, .unindexed⟩, ⟨"shares", .uint256, .unindexed⟩] },
  { name := "Repay",                params := [⟨"id",          .uint256, .unindexed⟩, ⟨"sender", .address, .unindexed⟩, ⟨"onBehalf", .address, .unindexed⟩, ⟨"assets", .uint256, .unindexed⟩, ⟨"shares", .uint256, .unindexed⟩] },
  { name := "Liquidate",            params := [⟨"id",          .uint256, .unindexed⟩, ⟨"sender", .address, .unindexed⟩, ⟨"borrower", .address, .unindexed⟩, ⟨"repaidAssets", .uint256, .unindexed⟩, ⟨"repaidShares", .uint256, .unindexed⟩, ⟨"seizedAssets", .uint256, .unindexed⟩, ⟨"badDebtAssets", .uint256, .unindexed⟩, ⟨"badDebtShares", .uint256, .unindexed⟩] },
  { name := "AccrueInterest",       params := [⟨"id",          .uint256, .unindexed⟩, ⟨"prevBorrowRate", .uint256, .unindexed⟩, ⟨"interest", .uint256, .unindexed⟩, ⟨"feeShares", .uint256, .unindexed⟩] },
  { name := "FlashLoan",            params := [⟨"sender",      .address, .unindexed⟩, ⟨"token",  .address, .unindexed⟩, ⟨"assets", .uint256, .unindexed⟩] }
]

def morphoGeneratedSpec : CompilationModel :=
  { Morpho.Compiler.MacroSlice.MorphoViewSlice.spec with
      name := "Morpho"
      events := morphoEvents
      externals := [
        { name := "keccakMarketParams"
          params := [.uint256, .uint256, .uint256, .uint256, .uint256]
          returnType := some .uint256
          returns := [.uint256]
          axiomNames := ["market_id_deterministic"] },
        { name := "borrowRate"
          params := [.uint256, .uint256]
          returnType := some .uint256
          returns := [.uint256]
          axiomNames := [] },
        { name := "collateralPrice"
          params := [.uint256]
          returnType := some .uint256
          returns := [.uint256]
          axiomNames := [] },
        { name := "oraclePrice"
          params := [.uint256]
          returnType := some .uint256
          returns := [.uint256]
          axiomNames := [] }
      ] }

/--
Canonical selector boundary derived from the generated spec.

This removes the compiler's runtime dependency on manually maintained selector
lists in `Spec.lean`: selectors are computed from function signatures of the
single spec source consumed by compilation.
-/
def morphoGeneratedSelectors : IO (List Nat) :=
  _root_.Compiler.Selector.computeSelectors morphoGeneratedSpec

end Morpho.Compiler.Generated
