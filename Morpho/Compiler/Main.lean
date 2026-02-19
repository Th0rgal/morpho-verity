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

private def idToMarketParamsCase : String := "\
            case 0x2c3c9157 {\n\
                /* idToMarketParams(bytes32) */\n\
                if callvalue() {\n\
                    revert(0, 0)\n\
                }\n\
                if lt(calldatasize(), 36) {\n\
                    revert(0, 0)\n\
                }\n\
                let id := calldataload(4)\n\
                let paramsBase := mappingSlot(8, id)\n\
                mstore(0, sload(paramsBase))\n\
                mstore(32, sload(add(paramsBase, 1)))\n\
                mstore(64, sload(add(paramsBase, 2)))\n\
                mstore(96, sload(add(paramsBase, 3)))\n\
                mstore(128, sload(add(paramsBase, 4)))\n\
                return(0, 160)\n\
            }\n"

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
                let marketSupplyWord := sload(mappingSlot(3, id))\n\
                let totalSupplyAssets := and(marketSupplyWord, 0xffffffffffffffffffffffffffffffff)\n\
                let totalSupplyShares := shr(128, marketSupplyWord)\n\
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
                let positionBase := mappingSlot(mappingSlot(2, id), onBehalf)\n\
                sstore(positionBase, add(sload(positionBase), sharesSupplied))\n\
                let newTotalSupplyAssets := add(totalSupplyAssets, assetsSupplied)\n\
                let newTotalSupplyShares := add(totalSupplyShares, sharesSupplied)\n\
                sstore(mappingSlot(3, id), or(and(newTotalSupplyAssets, 0xffffffffffffffffffffffffffffffff), shl(128, and(newTotalSupplyShares, 0xffffffffffffffffffffffffffffffff))))\n\
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

private def injectIdToMarketParamsShim (text : String) : Except String String := do
  let needle := "            default {\n                revert(0, 0)\n            }\n"
  let patched := text.replace needle (idToMarketParamsCase ++ needle)
  if patched != text then
    pure patched
  else
    throw "Could not inject idToMarketParams shim: default dispatch branch not found"

private def replaceOrThrow (text oldFragment newFragment : String) (label : String) : Except String String := do
  let patched := text.replace oldFragment newFragment
  if patched != text then pure patched
  else throw s!"Could not patch Yul output for {label}"

private def injectEventLogs (text : String) : Except String String := do
  let constructorOld := "        sstore(0, arg0)\n        sstore(1, 0)\n"
  let constructorNew := "        sstore(0, arg0)\n        sstore(1, 0)\n        log2(0, 0, 0x167d3e9c1016ab80e58802ca9da10ce5c6a0f4debc46a2e7a2cd9e56899a4fb5, arg0)\n"
  let t1 ← replaceOrThrow text constructorOld constructorNew "constructor SetOwner event"

  let setOwnerOld := "                sstore(0, newOwner)\n                stop()\n"
  let setOwnerNew := "                sstore(0, newOwner)\n                log2(0, 0, 0x167d3e9c1016ab80e58802ca9da10ce5c6a0f4debc46a2e7a2cd9e56899a4fb5, newOwner)\n                stop()\n"
  let t2 ← replaceOrThrow t1 setOwnerOld setOwnerNew "setOwner event"

  let enableIrmOld := "                sstore(mappingSlot(2, irm), 1)\n                stop()\n"
  let enableIrmNew := "                sstore(mappingSlot(2, irm), 1)\n                log2(0, 0, 0x590e04cdebeccba40f566186b9746ad295a4cd358ea4fefaaea6ce79630d96c0, irm)\n                stop()\n"
  let t3 ← replaceOrThrow t2 enableIrmOld enableIrmNew "enableIrm event"

  let enableLltvOld := "                sstore(mappingSlot(3, lltv), 1)\n                stop()\n"
  let enableLltvNew := "                sstore(mappingSlot(3, lltv), 1)\n                mstore(0, lltv)\n                log1(0, 32, 0x297b80e7a896fad470c630f6575072d609bde997260ff3db851939405ec29139)\n                stop()\n"
  let t4 ← replaceOrThrow t3 enableLltvOld enableLltvNew "enableLltv event"

  let setFeeRecipientOld := "                sstore(1, newFeeRecipient)\n                stop()\n"
  let setFeeRecipientNew := "                sstore(1, newFeeRecipient)\n                log2(0, 0, 0x2e979f80fe4d43055c584cf4a8467c55875ea36728fc37176c05acd784eb7a73, newFeeRecipient)\n                stop()\n"
  let t5 ← replaceOrThrow t4 setFeeRecipientOld setFeeRecipientNew "setFeeRecipient event"

  let setFeeOld := "                sstore(mappingSlot(7, id), newFee)\n                stop()\n"
  let setFeeNew := "                sstore(mappingSlot(7, id), newFee)\n                let __marketSlot := add(mappingSlot(3, id), 2)\n                let __packed := sload(__marketSlot)\n                sstore(__marketSlot, or(and(__packed, 0x00000000000000000000000000000000ffffffffffffffffffffffffffffffff), shl(128, newFee)))\n                mstore(0, newFee)\n                log2(0, 32, 0x139d6f58e9a127229667c8e3b36e88890a66cfc8ab1024ddc513e189e125b75b, id)\n                stop()\n"
  let t6 ← replaceOrThrow t5 setFeeOld setFeeNew "setFee event+packed slot"

  let createMarketOld := "                sstore(mappingSlot(6, id), timestamp())\n                sstore(mappingSlot(7, id), 0)\n                sstore(mappingSlot(8, id), 0)\n                sstore(mappingSlot(9, id), 0)\n                sstore(mappingSlot(10, id), 0)\n                sstore(mappingSlot(11, id), 0)\n                sstore(mappingSlot(12, id), loanToken)\n                sstore(mappingSlot(13, id), collateralToken)\n                sstore(mappingSlot(14, id), oracle)\n                sstore(mappingSlot(15, id), irm)\n                sstore(mappingSlot(16, id), lltv)\n                stop()\n"
  let createMarketNew := "                sstore(mappingSlot(6, id), timestamp())\n                sstore(mappingSlot(7, id), 0)\n                sstore(mappingSlot(8, id), 0)\n                sstore(mappingSlot(9, id), 0)\n                sstore(mappingSlot(10, id), 0)\n                sstore(mappingSlot(11, id), 0)\n                sstore(mappingSlot(12, id), loanToken)\n                sstore(mappingSlot(13, id), collateralToken)\n                sstore(mappingSlot(14, id), oracle)\n                sstore(mappingSlot(15, id), irm)\n                sstore(mappingSlot(16, id), lltv)\n                let __marketBase := mappingSlot(3, id)\n                sstore(__marketBase, 0)\n                sstore(add(__marketBase, 1), 0)\n                sstore(add(__marketBase, 2), timestamp())\n                let __paramsBase := mappingSlot(8, id)\n                sstore(__paramsBase, loanToken)\n                sstore(add(__paramsBase, 1), collateralToken)\n                sstore(add(__paramsBase, 2), oracle)\n                sstore(add(__paramsBase, 3), irm)\n                sstore(add(__paramsBase, 4), lltv)\n                mstore(0, loanToken)\n                mstore(32, collateralToken)\n                mstore(64, oracle)\n                mstore(96, irm)\n                mstore(128, lltv)\n                log2(0, 160, 0xac4b2400f169220b0c0afdde7a0b32e775ba727ea1cb30b35f935cdaab8683ac, id)\n                stop()\n"
  replaceOrThrow t6 createMarketOld createMarketNew "createMarket packed slots"

private def injectGetterShims (text : String) : Except String String := do
  let totalSupplyAssetsOld := "                let id := calldataload(4)\n                mstore(0, sload(mappingSlot(8, id)))\n                return(0, 32)\n"
  let totalSupplyAssetsNew := "                let id := calldataload(4)\n                let __marketWord := sload(mappingSlot(3, id))\n                mstore(0, and(__marketWord, 0xffffffffffffffffffffffffffffffff))\n                return(0, 32)\n"
  let t1 ← replaceOrThrow text totalSupplyAssetsOld totalSupplyAssetsNew "totalSupplyAssets packed getter"

  let totalSupplySharesOld := "                let id := calldataload(4)\n                mstore(0, sload(mappingSlot(9, id)))\n                return(0, 32)\n"
  let totalSupplySharesNew := "                let id := calldataload(4)\n                let __marketWord := sload(mappingSlot(3, id))\n                mstore(0, shr(128, __marketWord))\n                return(0, 32)\n"
  replaceOrThrow t1 totalSupplySharesOld totalSupplySharesNew "totalSupplyShares packed getter"

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
  let withIdToParams ← orThrow (injectIdToMarketParamsShim withSupply)
  let withGetters ← orThrow (injectGetterShims withIdToParams)
  let text ← orThrow (injectEventLogs withGetters)

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
