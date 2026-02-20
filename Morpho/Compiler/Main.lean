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

private def supplyCase : String := "\
            case 0xa99aad89 {\n\
                /* supply((address,address,address,address,uint256),uint256,uint256,address,bytes) */\n\
                if callvalue() {\n\
                    revert(0, 0)\n\
                }\n\
                if lt(calldatasize(), 292) {\n\
                    revert(0, 0)\n\
                }\n\
                let loanToken := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let collateralToken := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let oracle := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let irm := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let lltv := calldataload(132)\n\
                let assets := calldataload(164)\n\
                let shares := calldataload(196)\n\
                let onBehalf := and(calldataload(228), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let dataOffset := calldataload(260)\n\
                let id := keccakMarketParams(loanToken, collateralToken, oracle, irm, lltv)\n\
                if iszero(gt(sload(add(mappingSlot(3, id), 2)), 0)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 18)\n\
                    mstore(68, 0x6d61726b6574206e6f7420637265617465640000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if iszero(xor(iszero(assets), iszero(shares))) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 18)\n\
                    mstore(68, 0x696e636f6e73697374656e7420696e7075740000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if iszero(iszero(eq(onBehalf, 0))) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 12)\n\
                    mstore(68, 0x7a65726f20616464726573730000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                let totalSupplyAssets := sload(mappingSlot(8, id))\n\
                let totalSupplyShares := sload(mappingSlot(9, id))\n\
                let virtualShares := 1000000\n\
                let virtualAssets := 1\n\
                let denomShares := add(totalSupplyShares, virtualShares)\n\
                let denomAssets := add(totalSupplyAssets, virtualAssets)\n\
                let assetsSupplied := assets\n\
                let sharesSupplied := shares\n\
                if gt(assetsSupplied, 0) {\n\
                    sharesSupplied := div(mul(assetsSupplied, denomShares), denomAssets)\n\
                }\n\
                if gt(sharesSupplied, 0) {\n\
                    if iszero(gt(assetsSupplied, 0)) {\n\
                        assetsSupplied := div(add(mul(sharesSupplied, denomAssets), sub(denomShares, 1)), denomShares)\n\
                    }\n\
                }\n\
                let positionBase := mappingSlot(mappingSlot(17, id), onBehalf)\n\
                sstore(positionBase, add(sload(positionBase), sharesSupplied))\n\
                let newTotalSupplyAssets := add(totalSupplyAssets, assetsSupplied)\n\
                let newTotalSupplyShares := add(totalSupplyShares, sharesSupplied)\n\
                sstore(mappingSlot(8, id), newTotalSupplyAssets)\n\
                sstore(mappingSlot(9, id), newTotalSupplyShares)\n\
                mstore(0, caller())\n\
                mstore(32, assetsSupplied)\n\
                mstore(64, sharesSupplied)\n\
                log3(0, 96, 0xedf8870433c83823eb071d3df1caa8d008f12f6440918c20d75a3602cda30fe0, id, onBehalf)\n\
                if gt(dataOffset, sub(calldatasize(), 32)) {\n\
                    revert(0, 0)\n\
                }\n\
                let dataHead := add(4, dataOffset)\n\
                if gt(dataHead, sub(calldatasize(), 32)) {\n\
                    revert(0, 0)\n\
                }\n\
                let dataLen := calldataload(dataHead)\n\
                let dataStart := add(dataHead, 32)\n\
                if gt(dataLen, sub(calldatasize(), dataStart)) {\n\
                    revert(0, 0)\n\
                }\n\
                if gt(dataLen, 0) {\n\
                    mstore(0, 0x2075be0300000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, assetsSupplied)\n\
                    mstore(36, 64)\n\
                    mstore(68, dataLen)\n\
                    calldatacopy(100, dataStart, dataLen)\n\
                    if iszero(call(gas(), caller(), 0, 0, add(100, dataLen), 0, 0)) {\n\
                        returndatacopy(0, 0, returndatasize())\n\
                        revert(0, returndatasize())\n\
                    }\n\
                }\n\
                if iszero(extcodesize(loanToken)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 7)\n\
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                mstore(0, 0x23b872dd00000000000000000000000000000000000000000000000000000000)\n\
                mstore(4, caller())\n\
                mstore(36, address())\n\
                mstore(68, assetsSupplied)\n\
                if iszero(call(gas(), loanToken, 0, 0, 100, 0, 32)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 21)\n\
                    mstore(68, 0x7472616e7366657246726f6d2072657665727465640000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if eq(returndatasize(), 32) {\n\
                    returndatacopy(0, 0, 32)\n\
                    if iszero(mload(0)) {\n\
                        mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                        mstore(4, 32)\n\
                        mstore(36, 27)\n\
                        mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)\n\
                        revert(0, 100)\n\
                    }\n\
                }\n\
                mstore(0, assetsSupplied)\n\
                mstore(32, sharesSupplied)\n\
                return(0, 64)\n\
            }\n"

private def withdrawCase : String := "\
            case 0x5c2bea49 {\n\
                /* withdraw((address,address,address,address,uint256),uint256,uint256,address,address) */\n\
                if callvalue() {\n\
                    revert(0, 0)\n\
                }\n\
                if lt(calldatasize(), 292) {\n\
                    revert(0, 0)\n\
                }\n\
                let loanToken := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let collateralToken := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let oracle := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let irm := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let lltv := calldataload(132)\n\
                let assets := calldataload(164)\n\
                let shares := calldataload(196)\n\
                let onBehalf := and(calldataload(228), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let receiver := and(calldataload(260), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let id := keccakMarketParams(loanToken, collateralToken, oracle, irm, lltv)\n\
                if iszero(gt(sload(add(mappingSlot(3, id), 2)), 0)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 18)\n\
                    mstore(68, 0x6d61726b6574206e6f7420637265617465640000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if iszero(xor(iszero(assets), iszero(shares))) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 18)\n\
                    mstore(68, 0x696e636f6e73697374656e7420696e7075740000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if iszero(iszero(eq(receiver, 0))) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 12)\n\
                    mstore(68, 0x7a65726f20616464726573730000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if iszero(or(eq(caller(), onBehalf), eq(sload(mappingSlot(mappingSlot(4, onBehalf), caller())), 1))) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 14)\n\
                    mstore(68, 0x6e6f7420617574686f72697a6564000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                let totalSupplyAssets := sload(mappingSlot(8, id))\n\
                let totalSupplyShares := sload(mappingSlot(9, id))\n\
                let virtualShares := 1000000\n\
                let virtualAssets := 1\n\
                let denomShares := add(totalSupplyShares, virtualShares)\n\
                let denomAssets := add(totalSupplyAssets, virtualAssets)\n\
                let assetsWithdrawn := assets\n\
                let sharesWithdrawn := shares\n\
                if gt(assetsWithdrawn, 0) {\n\
                    sharesWithdrawn := div(add(mul(assetsWithdrawn, denomShares), sub(denomAssets, 1)), denomAssets)\n\
                }\n\
                if gt(sharesWithdrawn, 0) {\n\
                    if iszero(gt(assetsWithdrawn, 0)) {\n\
                        assetsWithdrawn := div(mul(sharesWithdrawn, denomAssets), denomShares)\n\
                    }\n\
                }\n\
                let positionBase := mappingSlot(mappingSlot(17, id), onBehalf)\n\
                let currentShares := sload(positionBase)\n\
                if lt(currentShares, sharesWithdrawn) {\n\
                    revert(0, 0)\n\
                }\n\
                if or(lt(totalSupplyAssets, assetsWithdrawn), lt(totalSupplyShares, sharesWithdrawn)) {\n\
                    revert(0, 0)\n\
                }\n\
                sstore(positionBase, sub(currentShares, sharesWithdrawn))\n\
                sstore(mappingSlot(8, id), sub(totalSupplyAssets, assetsWithdrawn))\n\
                sstore(mappingSlot(9, id), sub(totalSupplyShares, sharesWithdrawn))\n\
                mstore(0, caller())\n\
                mstore(32, assetsWithdrawn)\n\
                mstore(64, sharesWithdrawn)\n\
                log4(0, 96, 0xa56fc0ad5702ec05ce63666221f796fb62437c32db1aa1aa075fc6484cf58fbf, id, onBehalf, receiver)\n\
                if iszero(extcodesize(loanToken)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 7)\n\
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                mstore(0, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)\n\
                mstore(4, receiver)\n\
                mstore(36, assetsWithdrawn)\n\
                if iszero(call(gas(), loanToken, 0, 0, 68, 0, 32)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 17)\n\
                    mstore(68, 0x7472616e73666572207265766572746564000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if eq(returndatasize(), 32) {\n\
                    returndatacopy(0, 0, 32)\n\
                    if iszero(mload(0)) {\n\
                        mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                        mstore(4, 32)\n\
                        mstore(36, 23)\n\
                        mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)\n\
                        revert(0, 100)\n\
                    }\n\
                }\n\
                mstore(0, assetsWithdrawn)\n\
                mstore(32, sharesWithdrawn)\n\
                return(0, 64)\n\
            }\n"

private def injectSupplyShim (text : String) : Except String String := do
  let needle := "            default {\n                revert(0, 0)\n            }\n"
  let patched := text.replace needle (supplyCase ++ withdrawCase ++ needle)
  if patched != text then
    pure patched
  else
    throw "Could not inject supply shim: default dispatch branch not found"

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

private def injectStorageCompat (text : String) : Except String String := do
  let mappingSlotOld := "            function mappingSlot(baseSlot, key) -> slot {\n                mstore(0, key)\n                mstore(32, baseSlot)\n                slot := keccak256(0, 64)\n            }\n"
  let mappingSlotNew := "            function mappingSlot(baseSlot, key) -> slot {\n                mstore(0x200, key)\n                mstore(0x220, baseSlot)\n                slot := keccak256(0x200, 64)\n            }\n"
  let t0 ← replaceOrThrow text mappingSlotOld mappingSlotNew "mappingSlot scratch memory safety"

  let constructorOld := "        sstore(0, arg0)\n        sstore(1, 0)\n"
  let constructorNew := "        sstore(0, arg0)\n        sstore(1, 0)\n        log2(0, 0, 0x167d3e9c1016ab80e58802ca9da10ce5c6a0f4debc46a2e7a2cd9e56899a4fb5, arg0)\n"
  let t1 ← replaceOrThrow t0 constructorOld constructorNew "constructor SetOwner event"

  -- Morpho packs `marketFee` (high 128 bits) and `marketLastUpdate` (low 128 bits)
  -- in `markets[id].slot2`, so the Yul patch must keep this packed mirror in sync
  -- with the spec-level mapping writes used by the compiler.
  let setFeeOld := "sstore(mappingSlot(7, id), newFee)\n"
  let setFeePacked := packMarketFeeSlot2Expr "__packed" "newFee"
  let setFeeNew := "sstore(mappingSlot(7, id), newFee)\n                let __marketSlot := add(mappingSlot(3, id), 2)\n                let __packed := sload(__marketSlot)\n                sstore(__marketSlot, " ++ setFeePacked ++ ")\n"
  let t2 ← replaceOrThrow t1 setFeeOld setFeeNew "setFee packed slot compatibility"

  let createMarketOld := "sstore(mappingSlot(16, id), lltv)\n"
  let createMarketNew := "sstore(mappingSlot(16, id), lltv)\n                let __marketBase := mappingSlot(3, id)\n                sstore(__marketBase, 0)\n                sstore(add(__marketBase, 1), 0)\n                sstore(add(__marketBase, 2), timestamp())\n"
  let t3 ← replaceOrThrow t2 createMarketOld createMarketNew "createMarket packed slot compatibility"

  let supplyOld := "sstore(mappingSlot(9, id), newTotalSupplyShares)\nmstore(0, caller())\n"
  let supplySlot0Packed := packMarketSupplySlot0Expr "newTotalSupplyAssets" "newTotalSupplyShares"
  let supplyNew := s!"sstore(mappingSlot(9, id), newTotalSupplyShares)\nlet __marketSlot0 := mappingSlot(3, id)\nsstore(__marketSlot0, {supplySlot0Packed})\nmstore(0, caller())\n"
  let t4 ← replaceOrThrow t3 supplyOld supplyNew "supply packed slot compatibility"

  let withdrawOld := "sstore(mappingSlot(8, id), sub(totalSupplyAssets, assetsWithdrawn))\nsstore(mappingSlot(9, id), sub(totalSupplyShares, sharesWithdrawn))\nmstore(0, caller())\n"
  let withdrawSlot0Packed := packMarketSupplySlot0Expr "__newTotalSupplyAssets" "__newTotalSupplyShares"
  let withdrawNew := s!"let __newTotalSupplyAssets := sub(totalSupplyAssets, assetsWithdrawn)\nlet __newTotalSupplyShares := sub(totalSupplyShares, sharesWithdrawn)\nsstore(mappingSlot(8, id), __newTotalSupplyAssets)\nsstore(mappingSlot(9, id), __newTotalSupplyShares)\nlet __marketSlot0 := mappingSlot(3, id)\nsstore(__marketSlot0, {withdrawSlot0Packed})\nmstore(0, caller())\n"
  let t5 ← replaceOrThrow t4 withdrawOld withdrawNew "withdraw packed slot compatibility"

  let accrueOld := "let __ite_cond := gt(timestamp(), sload(mappingSlot(6, id)))\n                    if __ite_cond {\n                        sstore(mappingSlot(6, id), timestamp())\n"
  let accruePacked := packMarketLastUpdateSlot2Expr "__packed" "timestamp()"
  let accrueNew := "let __ite_cond := gt(timestamp(), sload(mappingSlot(6, id)))\n                    if __ite_cond {\n                        sstore(mappingSlot(6, id), timestamp())\n                        let __marketSlot := add(mappingSlot(3, id), 2)\n                        let __packed := sload(__marketSlot)\n                        sstore(__marketSlot, " ++ accruePacked ++ ")\n"
  replaceOrThrow t5 accrueOld accrueNew "accrueInterest packed slot compatibility"

private def writeContract (outDir : String) (contract : IRContract) (libraryPaths : List String) : IO Unit := do
  let yulObj := _root_.Compiler.emitYul contract
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
  let withSupply ← orThrow (injectSupplyShim baseText)
  let text ← orThrow (injectStorageCompat withSupply)

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
