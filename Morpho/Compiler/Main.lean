import Std
import Compiler.ContractSpec
import Compiler.Codegen
import Compiler.Yul.PrettyPrint
import Compiler.Linker
import Morpho.Compiler.Spec

namespace Morpho.Compiler.Main

open _root_.Compiler
open _root_.Compiler.ContractSpec
open _root_.Compiler.Yul
open _root_.Compiler.Linker

private def orThrow {α : Type} (r : Except String α) : IO α :=
  match r with
  | .ok v => pure v
  | .error e => throw (IO.userError e)

private def parseArgs (args : List String) : IO (String × List String × Bool) :=
  let rec go : List String → String → List String → Bool → IO (String × List String × Bool)
    | [], outDir, libs, verbose => pure (outDir, libs.reverse, verbose)
    | "--output" :: dir :: rest, _, libs, verbose => go rest dir libs verbose
    | "-o" :: dir :: rest, _, libs, verbose => go rest dir libs verbose
    | "--link" :: path :: rest, outDir, libs, verbose => go rest outDir (path :: libs) verbose
    | "--verbose" :: rest, outDir, libs, _ => go rest outDir libs true
    | "-v" :: rest, outDir, libs, _ => go rest outDir libs true
    | "--help" :: _, _, _, _
    | "-h" :: _, _, _, _ => do
      IO.println "Usage: morpho-verity-compiler [options]"
      IO.println ""
      IO.println "Options:"
      IO.println "  --output <dir>, -o <dir>    Output directory (default: compiler/yul)"
      IO.println "  --link <path>               Link external Yul library (repeatable)"
      IO.println "  --verbose, -v               Verbose logs"
      IO.println "  --help, -h                  Show this help"
      throw (IO.userError "help")
    | x :: _, _, _, _ =>
      throw (IO.userError s!"Unknown argument: {x}")
  go args "compiler/yul" [] false

-- ============================================================================
-- Post-codegen storage compatibility patches
--
-- The Morpho Blue Solidity contract uses a packed storage layout where
-- multiple fields share a single 256-bit slot. The DSL spec uses individual
-- mapping slots for each field (easier to reason about), so post-codegen
-- patches keep the packed Solidity layout in sync with the unpacked DSL
-- layout.
--
-- These patches will be eliminated once the Verity DSL supports packed
-- storage fields natively in the spec definition.
-- ============================================================================

private def replaceOrThrow (text oldFragment newFragment : String) (label : String) : Except String String := do
  let patched := text.replace oldFragment newFragment
  if patched != text then pure patched
  else throw s!"Could not patch Yul output for {label}"

private def lower128Mask : String := "0xffffffffffffffffffffffffffffffff"
private def upper128Mask : String := "0xffffffffffffffffffffffffffffffff00000000000000000000000000000000"

private def packMarketSupplySlot0Expr (totalSupplyAssets totalSupplyShares : String) : String :=
  s!"or(and({totalSupplyAssets}, {lower128Mask}), shl(128, and({totalSupplyShares}, {lower128Mask})))"

private def packMarketFeeSlot2Expr (packed fee : String) : String :=
  s!"or(and({packed}, {lower128Mask}), shl(128, {fee}))"

private def packMarketLastUpdateSlot2Expr (packed lastUpdate : String) : String :=
  s!"or(and({packed}, {upper128Mask}), and({lastUpdate}, {lower128Mask}))"

/-- Yul code-generation helper for the accrueInterest compat block.
    This inlines the full interest-rate-model call, Taylor series approximation,
    fee distribution, and packed storage writes that the DSL's simplified
    accrueInterest stub gets replaced with via post-codegen patching. -/
private def accrueInterestCompatBlock : String := "\
                let __prevLastUpdateAccrue := sload(mappingSlot(6, id))\n\
                if gt(timestamp(), __prevLastUpdateAccrue) {\n\
                    let elapsed := sub(timestamp(), __prevLastUpdateAccrue)\n\
                    sstore(mappingSlot(6, id), timestamp())\n\
                    let __marketSlot2Accrue := add(mappingSlot(3, id), 2)\n\
                    let __packed2Accrue := sload(__marketSlot2Accrue)\n\
                    sstore(__marketSlot2Accrue, or(and(__packed2Accrue, 0xffffffffffffffffffffffffffffffff00000000000000000000000000000000), and(timestamp(), 0xffffffffffffffffffffffffffffffff)))\n\
                    if iszero(eq(irm, 0)) {\n\
                        let totalBorrowAssetsAccrue := sload(mappingSlot(10, id))\n\
                        let totalBorrowSharesAccrue := sload(mappingSlot(11, id))\n\
                        let totalSupplyAssetsAccrue := sload(mappingSlot(8, id))\n\
                        let totalSupplySharesAccrue := sload(mappingSlot(9, id))\n\
                        let feeAccrue := and(sload(mappingSlot(7, id)), 0xffffffffffffffffffffffffffffffff)\n\
                        let borrowRateAccrue := 0\n\
                        mstore(0, 0x9451fed400000000000000000000000000000000000000000000000000000000)\n\
                        mstore(4, loanToken)\n\
                        mstore(36, collateralToken)\n\
                        mstore(68, oracle)\n\
                        mstore(100, irm)\n\
                        mstore(132, lltv)\n\
                        mstore(164, and(totalSupplyAssetsAccrue, 0xffffffffffffffffffffffffffffffff))\n\
                        mstore(196, and(totalSupplySharesAccrue, 0xffffffffffffffffffffffffffffffff))\n\
                        mstore(228, and(totalBorrowAssetsAccrue, 0xffffffffffffffffffffffffffffffff))\n\
                        mstore(260, and(totalBorrowSharesAccrue, 0xffffffffffffffffffffffffffffffff))\n\
                        mstore(292, and(__prevLastUpdateAccrue, 0xffffffffffffffffffffffffffffffff))\n\
                        mstore(324, feeAccrue)\n\
                        if iszero(call(gas(), irm, 0, 0, 356, 0, 32)) {\n\
                            returndatacopy(0, 0, returndatasize())\n\
                            revert(0, returndatasize())\n\
                        }\n\
                        if lt(returndatasize(), 32) {\n\
                            revert(0, 0)\n\
                        }\n\
                        returndatacopy(0, 0, 32)\n\
                        borrowRateAccrue := mload(0)\n\
                        let firstTermAccrue := mul(borrowRateAccrue, elapsed)\n\
                        let secondTermAccrue := div(mul(firstTermAccrue, firstTermAccrue), 2000000000000000000)\n\
                        let thirdTermAccrue := div(mul(secondTermAccrue, firstTermAccrue), 3000000000000000000)\n\
                        let growthAccrue := add(firstTermAccrue, add(secondTermAccrue, thirdTermAccrue))\n\
                        let interestAccrue := div(mul(totalBorrowAssetsAccrue, growthAccrue), 1000000000000000000)\n\
                        let newTotalBorrowAssetsAccrue := add(totalBorrowAssetsAccrue, interestAccrue)\n\
                        let newTotalSupplyAssetsAccrue := add(totalSupplyAssetsAccrue, interestAccrue)\n\
                        sstore(mappingSlot(10, id), newTotalBorrowAssetsAccrue)\n\
                        sstore(mappingSlot(8, id), newTotalSupplyAssetsAccrue)\n\
                        let feeSharesAccrue := 0\n\
                        let newTotalSupplySharesAccrue := totalSupplySharesAccrue\n\
                        if gt(feeAccrue, 0) {\n\
                            let feeAmountAccrue := div(mul(interestAccrue, feeAccrue), 1000000000000000000)\n\
                            let feeDenominatorAccrue := sub(newTotalSupplyAssetsAccrue, feeAmountAccrue)\n\
                            feeSharesAccrue := div(mul(feeAmountAccrue, add(totalSupplySharesAccrue, 1000000)), add(feeDenominatorAccrue, 1))\n\
                            let feeRecipientAccrue := and(sload(1), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                            let feePosSlotAccrue := mappingSlot(mappingSlot(17, id), feeRecipientAccrue)\n\
                            sstore(feePosSlotAccrue, add(sload(feePosSlotAccrue), feeSharesAccrue))\n\
                            let feePosCompatAccrue := mappingSlot(mappingSlot(2, id), feeRecipientAccrue)\n\
                            sstore(feePosCompatAccrue, add(sload(feePosCompatAccrue), feeSharesAccrue))\n\
                            newTotalSupplySharesAccrue := add(totalSupplySharesAccrue, feeSharesAccrue)\n\
                            sstore(mappingSlot(9, id), newTotalSupplySharesAccrue)\n\
                        }\n\
                        let __marketSlot0Accrue := mappingSlot(3, id)\n\
                        sstore(__marketSlot0Accrue, or(and(newTotalSupplyAssetsAccrue, 0xffffffffffffffffffffffffffffffff), shl(128, and(newTotalSupplySharesAccrue, 0xffffffffffffffffffffffffffffffff))))\n\
                        let __marketSlot1Accrue := add(__marketSlot0Accrue, 1)\n\
                        sstore(__marketSlot1Accrue, or(and(newTotalBorrowAssetsAccrue, 0xffffffffffffffffffffffffffffffff), shl(128, and(totalBorrowSharesAccrue, 0xffffffffffffffffffffffffffffffff))))\n\
                        mstore(0, borrowRateAccrue)\n\
                        mstore(32, interestAccrue)\n\
                        mstore(64, feeSharesAccrue)\n\
                        log2(0, 96, 0x9d9bd501d0657d7dfe415f779a620a62b78bc508ddc0891fbbd8b7ac0f8fce87, id)\n\
                    }\n\
                }\n"

private def morphoEmitOptions : _root_.Compiler.YulEmitOptions :=
  { backendProfile := .semantic
    patchConfig := { enabled := false, maxIterations := 2 }
    mappingSlotScratchBase := 0x200 }

private def injectStorageCompat (text : String) : Except String String := do
  -- Morpho Blue calls _accrueInterest(marketParams, id) in setFee before updating
  -- the fee. Inject the accrueInterest compat block before the max-fee check.
  let setFeeAccrueOld := "                if gt(newFee, 250000000000000000) {\n"
  let setFeeAccrueNew := accrueInterestCompatBlock ++ "                if gt(newFee, 250000000000000000) {\n"
  let t1b ← replaceOrThrow text setFeeAccrueOld setFeeAccrueNew "setFee accrueInterest injection"

  -- Morpho packs `marketFee` (high 128 bits) and `marketLastUpdate` (low 128 bits)
  -- in `markets[id].slot2`, so the Yul patch must keep this packed mirror in sync
  -- with the spec-level mapping writes used by the compiler.
  let setFeeOld := "sstore(mappingSlot(7, id), newFee)\n"
  let setFeePacked := packMarketFeeSlot2Expr "__packed" "newFee"
  let setFeeNew := "sstore(mappingSlot(7, id), newFee)\n                let __marketSlot := add(mappingSlot(3, id), 2)\n                let __packed := sload(__marketSlot)\n                sstore(__marketSlot, " ++ setFeePacked ++ ")\n"
  let t2 ← replaceOrThrow t1b setFeeOld setFeeNew "setFee packed slot compatibility"

  -- Patch the accrueInterest function's DSL-generated stub with full IRM + interest math
  let accrueOldLegacy := "let __ite_cond := gt(timestamp(), sload(mappingSlot(6, id)))\n                    if __ite_cond {\n                        sstore(mappingSlot(6, id), timestamp())\n"
  let accrueOldRuntime := "let __ite_cond := gt(timestamp(), sload(mappingSlot(6, id)))\n                            if __ite_cond {\n                                sstore(mappingSlot(6, id), timestamp())\n"
  let accruePacked := packMarketLastUpdateSlot2Expr "__packed" "timestamp()"
  let accrueNew := "let __prevLastUpdate := sload(mappingSlot(6, id))\n                    let __ite_cond := gt(timestamp(), __prevLastUpdate)\n                    if __ite_cond {\n                        sstore(mappingSlot(6, id), timestamp())\n                        let __marketSlot := add(mappingSlot(3, id), 2)\n                        let __packed := sload(__marketSlot)\n                        sstore(__marketSlot, " ++ accruePacked ++ ")\n"
  let t6Candidate := t2.replace accrueOldLegacy accrueNew
  let t6 ←
    if t6Candidate != t2 then
      pure t6Candidate
    else
      let t6Runtime := t2.replace accrueOldRuntime accrueNew
      if t6Runtime != t2 then
        pure t6Runtime
      else
        throw "Could not patch Yul output for accrueInterest packed slot compatibility"

  -- Replace the DSL's placeholder AccrueInterest event with full IRM call + interest logic
  let accrueEventOldLegacy :=
    "if iszero(eq(irm, 0)) {\n                            {\n                                let __evt_ptr := mload(64)\n                                mstore(add(__evt_ptr, 0), 0x416363727565496e74657265737428627974657333322c75696e743235362c75)\n                                mstore(add(__evt_ptr, 32), 0x696e743235362c75696e74323536290000000000000000000000000000000000)\n                                let __evt_topic0 := keccak256(__evt_ptr, 47)\n                                mstore(add(__evt_ptr, 0), 0)\n                                mstore(add(__evt_ptr, 32), 0)\n                                mstore(add(__evt_ptr, 64), 0)\n                                log2(__evt_ptr, 96, __evt_topic0, id)\n                            }\n                        }\n"
  let accrueEventOldDeeper :=
    "if iszero(eq(irm, 0)) {\n                                    {\n                                        let __evt_ptr := mload(64)\n                                        mstore(add(__evt_ptr, 0), 0x416363727565496e74657265737428627974657333322c75696e743235362c75)\n                                        mstore(add(__evt_ptr, 32), 0x696e743235362c75696e74323536290000000000000000000000000000000000)\n                                        let __evt_topic0 := keccak256(__evt_ptr, 47)\n                                        mstore(add(__evt_ptr, 0), 0)\n                                        mstore(add(__evt_ptr, 32), 0)\n                                        mstore(add(__evt_ptr, 64), 0)\n                                        log2(__evt_ptr, 96, __evt_topic0, id)\n                                    }\n                                }\n"
  let accrueEventNew :=
    "if iszero(eq(irm, 0)) {\n                            let totalBorrowAssets := sload(mappingSlot(10, id))\n                            let totalBorrowShares := sload(mappingSlot(11, id))\n                            let totalSupplyAssets := sload(mappingSlot(8, id))\n                            let totalSupplyShares := sload(mappingSlot(9, id))\n                            let fee := and(sload(mappingSlot(7, id)), 0xffffffffffffffffffffffffffffffff)\n                            mstore(0, 0x9451fed400000000000000000000000000000000000000000000000000000000)\n                            mstore(4, loanToken)\n                            mstore(36, collateralToken)\n                            mstore(68, oracle)\n                            mstore(100, irm)\n                            mstore(132, lltv)\n                            mstore(164, and(totalSupplyAssets, 0xffffffffffffffffffffffffffffffff))\n                            mstore(196, and(totalSupplyShares, 0xffffffffffffffffffffffffffffffff))\n                            mstore(228, and(totalBorrowAssets, 0xffffffffffffffffffffffffffffffff))\n                            mstore(260, and(totalBorrowShares, 0xffffffffffffffffffffffffffffffff))\n                            mstore(292, and(__prevLastUpdate, 0xffffffffffffffffffffffffffffffff))\n                            mstore(324, fee)\n                            if iszero(call(gas(), irm, 0, 0, 356, 0, 32)) {\n                                returndatacopy(0, 0, returndatasize())\n                                revert(0, returndatasize())\n                            }\n                            if lt(returndatasize(), 32) {\n                                revert(0, 0)\n                            }\n                            returndatacopy(0, 0, 32)\n                            let borrowRate := mload(0)\n                            let elapsed := sub(timestamp(), __prevLastUpdate)\n                            let firstTerm := mul(borrowRate, elapsed)\n                            let secondTerm := div(mul(firstTerm, firstTerm), 2000000000000000000)\n                            let thirdTerm := div(mul(secondTerm, firstTerm), 3000000000000000000)\n                            let growth := add(firstTerm, add(secondTerm, thirdTerm))\n                            let interest := div(mul(totalBorrowAssets, growth), 1000000000000000000)\n                            let newTotalBorrowAssets := add(totalBorrowAssets, interest)\n                            let newTotalSupplyAssets := add(totalSupplyAssets, interest)\n                            sstore(mappingSlot(10, id), newTotalBorrowAssets)\n                            sstore(mappingSlot(8, id), newTotalSupplyAssets)\n                            let feeShares := 0\n                            let newTotalSupplyShares := totalSupplyShares\n                            if gt(fee, 0) {\n                                let feeAmount := div(mul(interest, fee), 1000000000000000000)\n                                let feeDenominator := sub(newTotalSupplyAssets, feeAmount)\n                                feeShares := div(mul(feeAmount, add(totalSupplyShares, 1000000)), add(feeDenominator, 1))\n                                let feeRecipient := and(sload(1), 0xffffffffffffffffffffffffffffffffffffffff)\n                                let feePosSlot := mappingSlot(mappingSlot(17, id), feeRecipient)\n                                sstore(feePosSlot, add(sload(feePosSlot), feeShares))\n                                let feePosCompat := mappingSlot(mappingSlot(2, id), feeRecipient)\n                                sstore(feePosCompat, add(sload(feePosCompat), feeShares))\n                                newTotalSupplyShares := add(totalSupplyShares, feeShares)\n                                sstore(mappingSlot(9, id), newTotalSupplyShares)\n                            }\n                            let __marketSlot0 := mappingSlot(3, id)\n                            sstore(__marketSlot0, or(and(newTotalSupplyAssets, 0xffffffffffffffffffffffffffffffff), shl(128, and(newTotalSupplyShares, 0xffffffffffffffffffffffffffffffff))))\n                            sstore(add(__marketSlot0, 1), or(and(newTotalBorrowAssets, 0xffffffffffffffffffffffffffffffff), shl(128, and(totalBorrowShares, 0xffffffffffffffffffffffffffffffff))))\n                            mstore(0, borrowRate)\n                            mstore(32, interest)\n                            mstore(64, feeShares)\n                            log2(0, 96, 0x9d9bd501d0657d7dfe415f779a620a62b78bc508ddc0891fbbd8b7ac0f8fce87, id)\n                        }\n"
  let t7Candidate := t6.replace accrueEventOldLegacy accrueEventNew
  let t7 ←
    if t7Candidate != t6 then
      pure t7Candidate
    else
      let t7Deeper := t6.replace accrueEventOldDeeper accrueEventNew
      if t7Deeper != t6 then
        pure t7Deeper
      else
        throw "Could not patch Yul output for accrueInterest event and interest math"

  -- Remap the DSL's contiguous mapping slots 8-11 to the Solidity-compatible 20-23
  let t8 ← replaceOrThrow t7 "mappingSlot(8, id)" "mappingSlot(20, id)" "totalSupplyAssets storage remap"
  let t9 ← replaceOrThrow t8 "mappingSlot(9, id)" "mappingSlot(21, id)" "totalSupplyShares storage remap"
  let t10 ← replaceOrThrow t9 "mappingSlot(10, id)" "mappingSlot(22, id)" "totalBorrowAssets storage remap"
  let t11 ← replaceOrThrow t10 "mappingSlot(11, id)" "mappingSlot(23, id)" "totalBorrowShares storage remap"

  -- Initialize packed storage for createMarket (idToMarketParams at slots 8-12, market at slot 3)
  let createMarketOld := "sstore(mappingSlot(16, id), lltv)\n"
  let createMarketNew := "sstore(mappingSlot(16, id), lltv)\n                let __paramsBase := mappingSlot(8, id)\n                sstore(__paramsBase, loanToken)\n                sstore(add(__paramsBase, 1), collateralToken)\n                sstore(add(__paramsBase, 2), oracle)\n                sstore(add(__paramsBase, 3), irm)\n                sstore(add(__paramsBase, 4), lltv)\n                let __marketBase := mappingSlot(3, id)\n                sstore(__marketBase, 0)\n                sstore(add(__marketBase, 1), 0)\n                sstore(add(__marketBase, 2), timestamp())\n"
  let t12 ← replaceOrThrow t11 createMarketOld createMarketNew "createMarket storage initialization"

  -- Inject accrueInterest compat block into supply, withdraw, borrow, repay, liquidate, withdrawCollateral
  -- The DSL generates these functions with the `requireMarketCreated` check but without
  -- inline accrueInterest logic. We inject the full compat block after the market-created check.
  -- The operation functions use loanToken/collateralToken/oracle/irm/lltv which are in scope
  -- from the unpackMarketParams helper.
  --
  -- For supply: inject after "market not created" require, before "inconsistent input" require
  -- For all ops: inject the accrueInterest compat block before the main logic
  let injectAccrueOld := "market not created\n"
  let injectAccrueNew := "market not created\n" ++ accrueInterestCompatBlock
  -- This replaces ALL occurrences in the output (supply, withdraw, borrow, repay, liquidate,
  -- withdrawCollateral, setFee, accrueInterest itself) — the compat block is idempotent.
  let t13 := t12.replace injectAccrueOld injectAccrueNew

  -- Supply: packed slot0 sync after updating totals
  -- After DSL writes to mappingSlot(20, id) and mappingSlot(21, id), sync packed slot 3
  let supplyPackedOld := "sstore(mappingSlot(21, id), newTotalSupplyShares)\n"
  let supplySlot0Packed := packMarketSupplySlot0Expr "newTotalSupplyAssets" "newTotalSupplyShares"
  let supplyPackedNew := s!"sstore(mappingSlot(21, id), newTotalSupplyShares)\n                let __marketSlot0 := mappingSlot(3, id)\n                sstore(__marketSlot0, {supplySlot0Packed})\n"
  let t14 := t13.replace supplyPackedOld supplyPackedNew

  -- Supply: position compat sync (positionSupplyShares slot 2 mirror)
  -- After DSL writes to positionSupplyShares (slot 17), also write to compat slot 2
  let supplyPosSyncOld := "sstore(mappingSlot(mappingSlot(17, id), onBehalf), add(posShares, sharesSupplied))\n"
  let supplyPosSyncNew := "sstore(mappingSlot(mappingSlot(17, id), onBehalf), add(posShares, sharesSupplied))\n                let __posBaseCompat := mappingSlot(mappingSlot(2, id), onBehalf)\n                sstore(__posBaseCompat, add(sload(__posBaseCompat), sharesSupplied))\n"
  let t15 := t14.replace supplyPosSyncOld supplyPosSyncNew

  -- Withdraw: position compat sync + packed slot0 sync
  let withdrawPosSyncOld := "sstore(mappingSlot(mappingSlot(17, id), onBehalf), sub(posShares, sharesWithdrawn))\n"
  let withdrawPosSyncNew := "sstore(mappingSlot(mappingSlot(17, id), onBehalf), sub(posShares, sharesWithdrawn))\n                let __posBaseCompat := mappingSlot(mappingSlot(2, id), onBehalf)\n                let __currentSharesCompat := sload(__posBaseCompat)\n                if lt(__currentSharesCompat, sharesWithdrawn) {\n                    revert(0, 0)\n                }\n                sstore(__posBaseCompat, sub(__currentSharesCompat, sharesWithdrawn))\n"
  let t16 := t15.replace withdrawPosSyncOld withdrawPosSyncNew

  let withdrawTotalSyncOld := "sstore(mappingSlot(21, id), newTotalSupplyShares)\n                if ge(newTotalSupplyAssets"
  let withdrawSlot0Packed := packMarketSupplySlot0Expr "newTotalSupplyAssets" "newTotalSupplyShares"
  let withdrawTotalSyncNew := s!"sstore(mappingSlot(21, id), newTotalSupplyShares)\n                let __marketSlot0 := mappingSlot(3, id)\n                sstore(__marketSlot0, {withdrawSlot0Packed})\n                if ge(newTotalSupplyAssets"
  let t17 := t16.replace withdrawTotalSyncOld withdrawTotalSyncNew

  -- SupplyCollateral: position compat sync (packed borrowShares|collateral at slot2+1)
  let scPosSyncOld := "sstore(mappingSlot(mappingSlot(19, id), onBehalf), newCollateral)\n"
  let scPosSyncNew := "sstore(mappingSlot(mappingSlot(19, id), onBehalf), newCollateral)\n                let __posBaseCompat := mappingSlot(mappingSlot(2, id), onBehalf)\n                let __packedBorrowCollateral := sload(add(__posBaseCompat, 1))\n                let __borrowSharesCompat := and(__packedBorrowCollateral, 0xffffffffffffffffffffffffffffffff)\n                sstore(add(__posBaseCompat, 1), or(__borrowSharesCompat, shl(128, and(newCollateral, 0xffffffffffffffffffffffffffffffff))))\n"
  let t18 := t17.replace scPosSyncOld scPosSyncNew

  -- WithdrawCollateral: position compat sync
  let wcPosSyncOld := "sstore(mappingSlot(mappingSlot(19, id), onBehalf), newCollateral)\n                let borrowShares"
  let wcPosSyncNew := "sstore(mappingSlot(mappingSlot(19, id), onBehalf), newCollateral)\n                let __borrowSharesWc := sload(mappingSlot(mappingSlot(18, id), onBehalf))\n                let __posBaseCompatWc := mappingSlot(mappingSlot(2, id), onBehalf)\n                sstore(add(__posBaseCompatWc, 1), or(and(__borrowSharesWc, 0xffffffffffffffffffffffffffffffff), shl(128, and(newCollateral, 0xffffffffffffffffffffffffffffffff))))\n                let borrowShares"
  let t19 := t18.replace wcPosSyncOld wcPosSyncNew

  -- Borrow: combined packed slot1 + position compat sync
  -- Use unique context: borrow adds to totals (add(..., assetsBorrowed))
  let borrowSlot1Packed := s!"or(and(newTotalBorrowAssets, {lower128Mask}), shl(128, and(newTotalBorrowShares, {lower128Mask})))"
  let borrowCombinedOld := "let newTotalBorrowAssets := add(totalBorrowAssets, assetsBorrowed)\n                        let newTotalBorrowShares := add(totalBorrowShares, sharesBorrowed)\n                        sstore(mappingSlot(22, id), newTotalBorrowAssets)\n                        sstore(mappingSlot(23, id), newTotalBorrowShares)\n                        let collateralValue := sload(mappingSlot(mappingSlot(19, id), onBehalf))\n"
  let borrowCombinedNew := s!"let newTotalBorrowAssets := add(totalBorrowAssets, assetsBorrowed)\n                        let newTotalBorrowShares := add(totalBorrowShares, sharesBorrowed)\n                        sstore(mappingSlot(22, id), newTotalBorrowAssets)\n                        sstore(mappingSlot(23, id), newTotalBorrowShares)\n                let __borrowMarketSlot1 := add(mappingSlot(3, id), 1)\n                sstore(__borrowMarketSlot1, {borrowSlot1Packed})\n                let __borrowPosCompat := add(mappingSlot(mappingSlot(2, id), onBehalf), 1)\n                        let collateralValue := sload(mappingSlot(mappingSlot(19, id), onBehalf))\n                sstore(__borrowPosCompat, or(and(newBorrowShares, {lower128Mask}), shl(128, and(collateralValue, {lower128Mask}))))\n"
  let t20 := t19.replace borrowCombinedOld borrowCombinedNew

  -- Repay: combined position compat + packed slot1 sync
  -- Use unique context: repay uses sub/mul(gt(...)) for totals
  let repayPosSyncOld := "sstore(mappingSlot(mappingSlot(18, id), onBehalf), newBorrowShares)\n                        let newTotalBorrowAssets := mul(gt(totalBorrowAssets, assetsRepaid), sub(totalBorrowAssets, assetsRepaid))\n                        let newTotalBorrowShares := sub(totalBorrowShares, sharesRepaid)\n                        sstore(mappingSlot(23, id), newTotalBorrowShares)\n                        sstore(mappingSlot(22, id), newTotalBorrowAssets)\n"
  let repayPosSyncNew := s!"sstore(mappingSlot(mappingSlot(18, id), onBehalf), newBorrowShares)\n                let __repayMarketSlot1 := add(mappingSlot(3, id), 1)\n                let __repayPosCompat := add(mappingSlot(mappingSlot(2, id), onBehalf), 1)\n                let __repayPackedBorrowColl := sload(__repayPosCompat)\n                let __repayCollateralCompat := and(shr(128, __repayPackedBorrowColl), {lower128Mask})\n                sstore(__repayPosCompat, or(and(newBorrowShares, {lower128Mask}), shl(128, __repayCollateralCompat)))\n                        let newTotalBorrowAssets := mul(gt(totalBorrowAssets, assetsRepaid), sub(totalBorrowAssets, assetsRepaid))\n                        let newTotalBorrowShares := sub(totalBorrowShares, sharesRepaid)\n                        sstore(mappingSlot(23, id), newTotalBorrowShares)\n                        sstore(mappingSlot(22, id), newTotalBorrowAssets)\n                sstore(__repayMarketSlot1, or(and(newTotalBorrowAssets, {lower128Mask}), shl(128, and(newTotalBorrowShares, {lower128Mask}))))\n"
  let t22 := t20.replace repayPosSyncOld repayPosSyncNew

  -- Liquidate: position compat sync + packed slot syncs
  -- Use unique context: liquidate uses borrower (not onBehalf)
  let liqPosSyncOld := "sstore(mappingSlot(mappingSlot(18, id), borrower), newBorrowerBorrowShares)\n                        sstore(mappingSlot(mappingSlot(19, id), borrower), newBorrowerCollateral)\n"
  let liqPosSyncNew := "sstore(mappingSlot(mappingSlot(18, id), borrower), newBorrowerBorrowShares)\n                        sstore(mappingSlot(mappingSlot(19, id), borrower), newBorrowerCollateral)\n                let __liqPosCompat := add(mappingSlot(mappingSlot(2, id), borrower), 1)\n                sstore(__liqPosCompat, or(and(newBorrowerBorrowShares, 0xffffffffffffffffffffffffffffffff), shl(128, and(newBorrowerCollateral, 0xffffffffffffffffffffffffffffffff))))\n"
  let t23 := t22.replace liqPosSyncOld liqPosSyncNew

  -- Liquidate: packed slot syncs after totals update
  -- Use unique context: liquidate writes totalSupplyAssets via sstore(mappingSlot(20, id), newTotalSupplyAssets)
  -- after writing totalBorrowAssets and totalBorrowShares
  -- Note: totalSupplySharesBefore was spilled to memory, so read from storage (slot 21)
  let liqTotalSyncOld := "sstore(mappingSlot(23, id), newTotalBorrowShares)\n                        sstore(mappingSlot(22, id), newTotalBorrowAssets)\n                        sstore(mappingSlot(20, id), newTotalSupplyAssets)\n"
  let liqSlot0Packed := packMarketSupplySlot0Expr "newTotalSupplyAssets" "sload(mappingSlot(21, id))"
  let liqSlot1Packed := s!"or(and(newTotalBorrowAssets, {lower128Mask}), shl(128, and(newTotalBorrowShares, {lower128Mask})))"
  let liqTotalSyncNew := s!"sstore(mappingSlot(23, id), newTotalBorrowShares)\n                        sstore(mappingSlot(22, id), newTotalBorrowAssets)\n                        sstore(mappingSlot(20, id), newTotalSupplyAssets)\n                let __liqMarketBase := mappingSlot(3, id)\n                sstore(__liqMarketBase, {liqSlot0Packed})\n                sstore(add(__liqMarketBase, 1), {liqSlot1Packed})\n"
  let t24 := t23.replace liqTotalSyncOld liqTotalSyncNew

  pure t24

private def writeContract (outDir : String) (contract : IRContract) (libraryPaths : List String) : IO Unit := do
  let yulObj := _root_.Compiler.emitYulWithOptions contract morphoEmitOptions
  let libraries ← libraryPaths.mapM loadLibrary
  let allLibFunctions := libraries.flatten

  if !allLibFunctions.isEmpty then
    let _ ← orThrow (validateNoDuplicateNames allLibFunctions)
    let _ ← orThrow (validateNoNameCollisions yulObj allLibFunctions)
  let _ ← orThrow (validateExternalReferences yulObj allLibFunctions)
  if !allLibFunctions.isEmpty then
    let _ ← orThrow (validateCallArity yulObj allLibFunctions)

  let baseText ←
    if allLibFunctions.isEmpty then
      pure (_root_.Compiler.Yul.render yulObj)
    else
      orThrow (renderWithLibraries yulObj allLibFunctions)
  let text ← orThrow (injectStorageCompat baseText)

  IO.FS.createDirAll outDir
  IO.FS.writeFile s!"{outDir}/{contract.name}.yul" text

def main (args : List String) : IO Unit := do
  try
    let (outDir, libs, verbose) ← parseArgs args
    if verbose then
      IO.println s!"Compiling Morpho ContractSpec to {outDir}"
      if !libs.isEmpty then
        IO.println s!"Linking {libs.length} external libraries"

    let ir ← orThrow (compile Morpho.Compiler.Spec.morphoSpec Morpho.Compiler.Spec.morphoSelectors)
    writeContract outDir ir libs

    if verbose then
      IO.println s!"✓ Wrote {outDir}/{ir.name}.yul"
  catch e =>
    if e.toString == "help" then
      pure ()
    else
      throw e

end Morpho.Compiler.Main
