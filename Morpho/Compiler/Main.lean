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
                mstore(0, assetsSupplied)\n\
                mstore(32, sharesSupplied)\n\
                log4(0, 64, 0xedf8870433c83823eb071d3df1caa8d008f12f6440918c20d75a3602cda30fe0, id, caller(), onBehalf)\n\
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
                        mstore(36, 25)\n\
                        mstore(68, 0x7472616e7366657246726f6d2072657475726e65642066616c73650000000000)\n\
                        revert(0, 100)\n\
                    }\n\
                }\n\
                mstore(0, assetsSupplied)\n\
                mstore(32, sharesSupplied)\n\
                return(0, 64)\n\
            }\n"

private def injectSupplyShim (text : String) : Except String String := do
  let needle := "            default {\n                revert(0, 0)\n            }\n"
  let patched := text.replace needle (supplyCase ++ needle)
  if patched != text then
    pure patched
  else
    throw "Could not inject supply shim: default dispatch branch not found"

private def replaceOrThrow (text oldFragment newFragment : String) (label : String) : Except String String := do
  let patched := text.replace oldFragment newFragment
  if patched != text then pure patched
  else throw s!"Could not patch Yul output for {label}"

private def injectStorageCompat (text : String) : Except String String := do
  let mappingSlotOld := "            function mappingSlot(baseSlot, key) -> slot {\n                mstore(0, key)\n                mstore(32, baseSlot)\n                slot := keccak256(0, 64)\n            }\n"
  let mappingSlotNew := "            function mappingSlot(baseSlot, key) -> slot {\n                mstore(0x200, key)\n                mstore(0x220, baseSlot)\n                slot := keccak256(0x200, 64)\n            }\n"
  let t0 ← replaceOrThrow text mappingSlotOld mappingSlotNew "mappingSlot scratch memory safety"

  let constructorOld := "        sstore(0, arg0)\n        sstore(1, 0)\n"
  let constructorNew := "        sstore(0, arg0)\n        sstore(1, 0)\n        log2(0, 0, 0x167d3e9c1016ab80e58802ca9da10ce5c6a0f4debc46a2e7a2cd9e56899a4fb5, arg0)\n"
  let t1 ← replaceOrThrow t0 constructorOld constructorNew "constructor SetOwner event"

  let setFeeOld := "sstore(mappingSlot(7, id), newFee)\n"
  let setFeeNew := "sstore(mappingSlot(7, id), newFee)\n                let __marketSlot := add(mappingSlot(3, id), 2)\n                let __packed := sload(__marketSlot)\n                sstore(__marketSlot, or(and(__packed, 0x00000000000000000000000000000000ffffffffffffffffffffffffffffffff), shl(128, newFee)))\n"
  let t2 ← replaceOrThrow t1 setFeeOld setFeeNew "setFee packed slot compatibility"

  let createMarketOld := "sstore(mappingSlot(16, id), lltv)\n"
  let createMarketNew := "sstore(mappingSlot(16, id), lltv)\n                let __marketBase := mappingSlot(3, id)\n                sstore(__marketBase, 0)\n                sstore(add(__marketBase, 1), 0)\n                sstore(add(__marketBase, 2), timestamp())\n"
  let t3 ← replaceOrThrow t2 createMarketOld createMarketNew "createMarket packed slot compatibility"

  let idToMarketParamsOld := "                let id := calldataload(4)\n                mstore(0, sload(mappingSlot(12, id)))\n                mstore(32, sload(mappingSlot(13, id)))\n                mstore(64, sload(mappingSlot(14, id)))\n                mstore(96, sload(mappingSlot(15, id)))\n                mstore(128, sload(mappingSlot(16, id)))\n                return(0, 160)\n"
  let idToMarketParamsNew := "                let id := calldataload(4)\n                let __loanToken := sload(mappingSlot(12, id))\n                let __collateralToken := sload(mappingSlot(13, id))\n                let __oracle := sload(mappingSlot(14, id))\n                let __irm := sload(mappingSlot(15, id))\n                let __lltv := sload(mappingSlot(16, id))\n                mstore(0, __loanToken)\n                mstore(32, __collateralToken)\n                mstore(64, __oracle)\n                mstore(96, __irm)\n                mstore(128, __lltv)\n                return(0, 160)\n"
  let t4 ← replaceOrThrow t3 idToMarketParamsOld idToMarketParamsNew "idToMarketParams multi-return safety"

  let marketOld := "                let id := calldataload(4)\n                mstore(0, sload(mappingSlot(8, id)))\n                mstore(32, sload(mappingSlot(9, id)))\n                mstore(64, sload(mappingSlot(10, id)))\n                mstore(96, sload(mappingSlot(11, id)))\n                mstore(128, sload(mappingSlot(6, id)))\n                mstore(160, sload(mappingSlot(7, id)))\n                return(0, 192)\n"
  let marketNew := "                let id := calldataload(4)\n                let __totalSupplyAssets := sload(mappingSlot(8, id))\n                let __totalSupplyShares := sload(mappingSlot(9, id))\n                let __totalBorrowAssets := sload(mappingSlot(10, id))\n                let __totalBorrowShares := sload(mappingSlot(11, id))\n                let __lastUpdate := sload(mappingSlot(6, id))\n                let __fee := sload(mappingSlot(7, id))\n                mstore(0, __totalSupplyAssets)\n                mstore(32, __totalSupplyShares)\n                mstore(64, __totalBorrowAssets)\n                mstore(96, __totalBorrowShares)\n                mstore(128, __lastUpdate)\n                mstore(160, __fee)\n                return(0, 192)\n"
  replaceOrThrow t4 marketOld marketNew "market multi-return safety"

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
