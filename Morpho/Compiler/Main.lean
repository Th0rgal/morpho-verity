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
                let __posBaseCompat := mappingSlot(mappingSlot(2, id), onBehalf)\n\
                sstore(__posBaseCompat, add(sload(__posBaseCompat), sharesSupplied))\n\
                let newTotalSupplyAssets := add(totalSupplyAssets, assetsSupplied)\n\
                let newTotalSupplyShares := add(totalSupplyShares, sharesSupplied)\n\
                sstore(mappingSlot(8, id), newTotalSupplyAssets)\n\
                sstore(mappingSlot(9, id), newTotalSupplyShares)\n\
                mstore(0, assetsSupplied)\n\
                mstore(32, sharesSupplied)\n\
                log4(0, 64, 0xedf8870433c83823eb071d3df1caa8d008f12f6440918c20d75a3602cda30fe0, id, and(caller(), 0xffffffffffffffffffffffffffffffffffffffff), onBehalf)\n\
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
                        /* transferFrom returned false length */\n\
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
                    mstore(36, 12)\n\
                    mstore(68, 0x756e617574686f72697a65640000000000000000000000000000000000000000)\n\
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
                let __posBaseCompat := mappingSlot(mappingSlot(2, id), onBehalf)\n\
                let __currentSharesCompat := sload(__posBaseCompat)\n\
                if lt(__currentSharesCompat, sharesWithdrawn) {\n\
                    revert(0, 0)\n\
                }\n\
                if or(lt(totalSupplyAssets, assetsWithdrawn), lt(totalSupplyShares, sharesWithdrawn)) {\n\
                    revert(0, 0)\n\
                }\n\
                sstore(positionBase, sub(currentShares, sharesWithdrawn))\n\
                sstore(__posBaseCompat, sub(__currentSharesCompat, sharesWithdrawn))\n\
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
                        /* transfer returned false length */\n\
                        mstore(36, 23)\n\
                        mstore(68, 0x7472616e736665722072657475726e65642066616c7365000000000000000000)\n\
                        revert(0, 100)\n\
                    }\n\
                }\n\
                mstore(0, assetsWithdrawn)\n\
                mstore(32, sharesWithdrawn)\n\
                return(0, 64)\n\
            }\n"

private def supplyCollateralCase : String := "\
            case 0x238d6579 {\n\
                /* supplyCollateral((address,address,address,address,uint256),uint256,address,bytes) */\n\
                if callvalue() {\n\
                    revert(0, 0)\n\
                }\n\
                if lt(calldatasize(), 260) {\n\
                    revert(0, 0)\n\
                }\n\
                let loanToken := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let collateralToken := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let oracle := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let irm := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let lltv := calldataload(132)\n\
                let assets := calldataload(164)\n\
                let onBehalf := and(calldataload(196), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let dataOffset := calldataload(228)\n\
                let id := keccakMarketParams(loanToken, collateralToken, oracle, irm, lltv)\n\
                if iszero(gt(sload(add(mappingSlot(3, id), 2)), 0)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 18)\n\
                    mstore(68, 0x6d61726b6574206e6f7420637265617465640000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if iszero(assets) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 11)\n\
                    mstore(68, 0x7a65726f20617373657473000000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if iszero(iszero(eq(onBehalf, 0))) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 12)\n\
                    mstore(68, 0x7a65726f20616464726573730000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                let positionBase := mappingSlot(mappingSlot(19, id), onBehalf)\n\
                let newCollateral := add(sload(positionBase), assets)\n\
                sstore(positionBase, newCollateral)\n\
                let __posBaseCompat := mappingSlot(mappingSlot(2, id), onBehalf)\n\
                let __packedBorrowCollateral := sload(add(__posBaseCompat, 1))\n\
                let __borrowSharesCompat := and(__packedBorrowCollateral, 0xffffffffffffffffffffffffffffffff)\n\
                sstore(add(__posBaseCompat, 1), or(__borrowSharesCompat, shl(128, and(newCollateral, 0xffffffffffffffffffffffffffffffff))))\n\
                mstore(0, assets)\n\
                log4(0, 32, 0xa3b9472a1399e17e123f3c2e6586c23e504184d504de59cdaa2b375e880c6184, id, and(caller(), 0xffffffffffffffffffffffffffffffffffffffff), onBehalf)\n\
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
                    mstore(0, 0xb1022fdf00000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, assets)\n\
                    mstore(36, 64)\n\
                    mstore(68, dataLen)\n\
                    calldatacopy(100, dataStart, dataLen)\n\
                    if iszero(call(gas(), caller(), 0, 0, add(100, dataLen), 0, 0)) {\n\
                        returndatacopy(0, 0, returndatasize())\n\
                        revert(0, returndatasize())\n\
                    }\n\
                }\n\
                if iszero(extcodesize(collateralToken)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 7)\n\
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                mstore(0, 0x23b872dd00000000000000000000000000000000000000000000000000000000)\n\
                mstore(4, caller())\n\
                mstore(36, address())\n\
                mstore(68, assets)\n\
                if iszero(call(gas(), collateralToken, 0, 0, 100, 0, 32)) {\n\
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
                stop()\n\
            }\n"

private def withdrawCollateralCase : String := "\
            case 0x8720316d {\n\
                /* withdrawCollateral((address,address,address,address,uint256),uint256,address,address) */\n\
                if callvalue() {\n\
                    revert(0, 0)\n\
                }\n\
                if lt(calldatasize(), 260) {\n\
                    revert(0, 0)\n\
                }\n\
                let loanToken := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let collateralToken := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let oracle := and(calldataload(68), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let irm := and(calldataload(100), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let lltv := calldataload(132)\n\
                let assets := calldataload(164)\n\
                let onBehalf := and(calldataload(196), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let receiver := and(calldataload(228), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let id := keccakMarketParams(loanToken, collateralToken, oracle, irm, lltv)\n\
                if iszero(gt(sload(add(mappingSlot(3, id), 2)), 0)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 18)\n\
                    mstore(68, 0x6d61726b6574206e6f7420637265617465640000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if iszero(assets) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 11)\n\
                    mstore(68, 0x7a65726f20617373657473000000000000000000000000000000000000000000)\n\
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
                    mstore(36, 12)\n\
                    mstore(68, 0x756e617574686f72697a65640000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                let positionBase := mappingSlot(mappingSlot(19, id), onBehalf)\n\
                let currentCollateral := sload(positionBase)\n\
                if lt(currentCollateral, assets) {\n\
                    revert(0, 0)\n\
                }\n\
                let newCollateral := sub(currentCollateral, assets)\n\
                sstore(positionBase, newCollateral)\n\
                let __posBaseCompat := mappingSlot(mappingSlot(2, id), onBehalf)\n\
                let __packedBorrowCollateral := sload(add(__posBaseCompat, 1))\n\
                let __borrowSharesCompat := and(__packedBorrowCollateral, 0xffffffffffffffffffffffffffffffff)\n\
                sstore(add(__posBaseCompat, 1), or(__borrowSharesCompat, shl(128, and(newCollateral, 0xffffffffffffffffffffffffffffffff))))\n\
                if gt(__borrowSharesCompat, 0) {\n\
                    if iszero(extcodesize(oracle)) {\n\
                        revert(0, 0)\n\
                    }\n\
                    mstore(0, 0xa035b1fe00000000000000000000000000000000000000000000000000000000)\n\
                    if iszero(staticcall(gas(), oracle, 0, 4, 0, 32)) {\n\
                        revert(0, 0)\n\
                    }\n\
                    let collateralPrice := mload(0)\n\
                    let totalBorrowAssets := sload(mappingSlot(10, id))\n\
                    let totalBorrowShares := sload(mappingSlot(11, id))\n\
                    let __denomBorrowShares := add(totalBorrowShares, 1000000)\n\
                    let borrowedAssets := div(add(mul(__borrowSharesCompat, add(totalBorrowAssets, 1)), sub(__denomBorrowShares, 1)), __denomBorrowShares)\n\
                    let maxBorrow := div(mul(div(mul(newCollateral, collateralPrice), 1000000000000000000000000000000000000), lltv), 1000000000000000000)\n\
                    if gt(borrowedAssets, maxBorrow) {\n\
                        mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                        mstore(4, 32)\n\
                        mstore(36, 23)\n\
                        mstore(68, 0x696e73756666696369656e7420636f6c6c61746572616c000000000000000000)\n\
                        revert(0, 100)\n\
                    }\n\
                }\n\
                mstore(0, caller())\n\
                mstore(32, assets)\n\
                log4(0, 64, 0xe80ebd7cc9223d7382aab2e0d1d6155c65651f83d53c8b9b06901d167e321142, id, onBehalf, receiver)\n\
                if iszero(extcodesize(collateralToken)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 7)\n\
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                mstore(0, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)\n\
                mstore(4, receiver)\n\
                mstore(36, assets)\n\
                if iszero(call(gas(), collateralToken, 0, 0, 68, 0, 32)) {\n\
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
                stop()\n\
            }\n"

private def borrowCase : String := "\
            case 0x50d8cd4b {\n\
                /* borrow((address,address,address,address,uint256),uint256,uint256,address,address) */\n\
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
                    mstore(36, 12)\n\
                    mstore(68, 0x756e617574686f72697a65640000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                let totalBorrowAssets := sload(mappingSlot(10, id))\n\
                let totalBorrowShares := sload(mappingSlot(11, id))\n\
                let denomShares := add(totalBorrowShares, 1000000)\n\
                let denomAssets := add(totalBorrowAssets, 1)\n\
                let assetsBorrowed := assets\n\
                let sharesBorrowed := shares\n\
                if gt(assetsBorrowed, 0) {\n\
                    sharesBorrowed := div(add(mul(assetsBorrowed, denomShares), sub(denomAssets, 1)), denomAssets)\n\
                }\n\
                if gt(sharesBorrowed, 0) {\n\
                    if iszero(gt(assetsBorrowed, 0)) {\n\
                        assetsBorrowed := div(mul(sharesBorrowed, denomAssets), denomShares)\n\
                    }\n\
                }\n\
                let posBorrowSlot := mappingSlot(mappingSlot(18, id), onBehalf)\n\
                let newBorrowShares := add(sload(posBorrowSlot), sharesBorrowed)\n\
                sstore(posBorrowSlot, newBorrowShares)\n\
                let newTotalBorrowAssets := add(totalBorrowAssets, assetsBorrowed)\n\
                let newTotalBorrowShares := add(totalBorrowShares, sharesBorrowed)\n\
                sstore(mappingSlot(10, id), newTotalBorrowAssets)\n\
                sstore(mappingSlot(11, id), newTotalBorrowShares)\n\
                let __marketSlot1 := add(mappingSlot(3, id), 1)\n\
                sstore(__marketSlot1, or(and(newTotalBorrowAssets, 0xffffffffffffffffffffffffffffffff), shl(128, and(newTotalBorrowShares, 0xffffffffffffffffffffffffffffffff))))\n\
                let posCollateralSlot := mappingSlot(mappingSlot(19, id), onBehalf)\n\
                let collateralValue := sload(posCollateralSlot)\n\
                let packedPosCompatSlot := add(mappingSlot(mappingSlot(2, id), onBehalf), 1)\n\
                sstore(packedPosCompatSlot, or(and(newBorrowShares, 0xffffffffffffffffffffffffffffffff), shl(128, and(collateralValue, 0xffffffffffffffffffffffffffffffff))))\n\
                if iszero(extcodesize(oracle)) {\n\
                    revert(0, 0)\n\
                }\n\
                mstore(0, 0xa035b1fe00000000000000000000000000000000000000000000000000000000)\n\
                if iszero(staticcall(gas(), oracle, 0, 4, 0, 32)) {\n\
                    revert(0, 0)\n\
                }\n\
                let collateralPrice := mload(0)\n\
                let borrowedAssets := div(mul(newBorrowShares, add(newTotalBorrowAssets, 1)), add(newTotalBorrowShares, 1000000))\n\
                let maxBorrow := div(mul(div(mul(collateralValue, collateralPrice), 1000000000000000000000000000000000000), lltv), 1000000000000000000)\n\
                if gt(borrowedAssets, maxBorrow) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 23)\n\
                    mstore(68, 0x696e73756666696369656e7420636f6c6c61746572616c000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                if gt(newTotalBorrowAssets, sload(mappingSlot(8, id))) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 22)\n\
                    mstore(68, 0x696e73756666696369656e74206c697175696469747900000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                mstore(0, caller())\n\
                mstore(32, assetsBorrowed)\n\
                mstore(64, sharesBorrowed)\n\
                log4(0, 96, 0x570954540bed6b1304a87dfe815a5eda4a648f7097a16240dcd85c9b5fd42a43, id, onBehalf, receiver)\n\
                if iszero(extcodesize(loanToken)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 7)\n\
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                mstore(0, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)\n\
                mstore(4, receiver)\n\
                mstore(36, assetsBorrowed)\n\
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
                mstore(0, assetsBorrowed)\n\
                mstore(32, sharesBorrowed)\n\
                return(0, 64)\n\
            }\n"

private def repayCase : String := "\
            case 0x20b76e81 {\n\
                /* repay((address,address,address,address,uint256),uint256,uint256,address,bytes) */\n\
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
                let totalBorrowAssets := sload(mappingSlot(10, id))\n\
                let totalBorrowShares := sload(mappingSlot(11, id))\n\
                let denomShares := add(totalBorrowShares, 1000000)\n\
                let denomAssets := add(totalBorrowAssets, 1)\n\
                let assetsRepaid := assets\n\
                let sharesRepaid := shares\n\
                if gt(assetsRepaid, 0) {\n\
                    sharesRepaid := div(mul(assetsRepaid, denomShares), denomAssets)\n\
                }\n\
                if gt(sharesRepaid, 0) {\n\
                    if iszero(gt(assetsRepaid, 0)) {\n\
                        assetsRepaid := div(add(mul(sharesRepaid, denomAssets), sub(denomShares, 1)), denomShares)\n\
                    }\n\
                }\n\
                let posBorrowSlot := mappingSlot(mappingSlot(18, id), onBehalf)\n\
                let currentBorrowShares := sload(posBorrowSlot)\n\
                if lt(currentBorrowShares, sharesRepaid) {\n\
                    revert(0, 0)\n\
                }\n\
                let newBorrowShares := sub(currentBorrowShares, sharesRepaid)\n\
                sstore(posBorrowSlot, newBorrowShares)\n\
                let newTotalBorrowShares := sub(totalBorrowShares, sharesRepaid)\n\
                let newTotalBorrowAssets := mul(gt(totalBorrowAssets, assetsRepaid), sub(totalBorrowAssets, assetsRepaid))\n\
                sstore(mappingSlot(11, id), newTotalBorrowShares)\n\
                sstore(mappingSlot(10, id), newTotalBorrowAssets)\n\
                let __marketSlot1 := add(mappingSlot(3, id), 1)\n\
                sstore(__marketSlot1, or(and(newTotalBorrowAssets, 0xffffffffffffffffffffffffffffffff), shl(128, and(newTotalBorrowShares, 0xffffffffffffffffffffffffffffffff))))\n\
                let packedPosCompatSlot := add(mappingSlot(mappingSlot(2, id), onBehalf), 1)\n\
                let __packedBorrowCollateral := sload(packedPosCompatSlot)\n\
                let __collateralCompat := and(shr(128, __packedBorrowCollateral), 0xffffffffffffffffffffffffffffffff)\n\
                sstore(packedPosCompatSlot, or(and(newBorrowShares, 0xffffffffffffffffffffffffffffffff), shl(128, __collateralCompat)))\n\
                mstore(0, assetsRepaid)\n\
                mstore(32, sharesRepaid)\n\
                log4(0, 64, 0x52acb05cebbd3cd39715469f22afbf5a17496295ef3bc9bb5944056c63ccaa09, id, and(caller(), 0xffffffffffffffffffffffffffffffffffffffff), onBehalf)\n\
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
                    mstore(0, 0x05b4591c00000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, assetsRepaid)\n\
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
                mstore(68, assetsRepaid)\n\
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
                mstore(0, assetsRepaid)\n\
                mstore(32, sharesRepaid)\n\
                return(0, 64)\n\
            }\n"

private def flashLoanCase : String := "\
            case 0xe0232b42 {\n\
                /* flashLoan(address,uint256,bytes) */\n\
                if callvalue() {\n\
                    revert(0, 0)\n\
                }\n\
                if lt(calldatasize(), 100) {\n\
                    revert(0, 0)\n\
                }\n\
                let token := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let assets := calldataload(36)\n\
                let dataOffset := calldataload(68)\n\
                if iszero(assets) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 11)\n\
                    mstore(68, 0x7a65726f20617373657473000000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                mstore(0, assets)\n\
                log3(0, 32, 0xc76f1b4fe4396ac07a9fa55a415d4ca430e72651d37d3401f3bed7cb13fc4f12, and(caller(), 0xffffffffffffffffffffffffffffffffffffffff), token)\n\
                if iszero(extcodesize(token)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 7)\n\
                    mstore(68, 0x6e6f20636f646500000000000000000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                mstore(0, 0xa9059cbb00000000000000000000000000000000000000000000000000000000)\n\
                mstore(4, caller())\n\
                mstore(36, assets)\n\
                if iszero(call(gas(), token, 0, 0, 68, 0, 32)) {\n\
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
                mstore(0, 0x31f5707200000000000000000000000000000000000000000000000000000000)\n\
                mstore(4, assets)\n\
                mstore(36, 64)\n\
                mstore(68, dataLen)\n\
                calldatacopy(100, dataStart, dataLen)\n\
                if iszero(call(gas(), caller(), 0, 0, add(100, dataLen), 0, 0)) {\n\
                    returndatacopy(0, 0, returndatasize())\n\
                    revert(0, returndatasize())\n\
                }\n\
                mstore(0, 0x23b872dd00000000000000000000000000000000000000000000000000000000)\n\
                mstore(4, caller())\n\
                mstore(36, address())\n\
                mstore(68, assets)\n\
                if iszero(call(gas(), token, 0, 0, 100, 0, 32)) {\n\
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
                stop()\n\
            }\n"

private def setAuthorizationWithSigCase : String := "\
            case 0x8069218f {\n\
                /* setAuthorizationWithSig((address,address,bool,uint256,uint256),(uint8,bytes32,bytes32)) */\n\
                if callvalue() {\n\
                    revert(0, 0)\n\
                }\n\
                if lt(calldatasize(), 260) {\n\
                    revert(0, 0)\n\
                }\n\
                let authorizer := and(calldataload(4), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let authorized := and(calldataload(36), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                let newIsAuthorized := and(calldataload(68), 1)\n\
                let expectedNonce := calldataload(100)\n\
                let deadline := calldataload(132)\n\
                let v := and(calldataload(164), 0xff)\n\
                let r := calldataload(196)\n\
                let s := calldataload(228)\n\
                if gt(timestamp(), deadline) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 17)\n\
                    mstore(68, 0x7369676e61747572652065787069726564000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                let nonceSlot := mappingSlot(5, authorizer)\n\
                let currentNonce := sload(nonceSlot)\n\
                if iszero(eq(expectedNonce, currentNonce)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 13)\n\
                    mstore(68, 0x696e76616c6964206e6f6e636500000000000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                sstore(nonceSlot, add(currentNonce, 1))\n\
                mstore(0, 0x81d0284fb0e2cde18d0553b06189d6f7613c96a01bb5b5e7828eade6a0dcac91)\n\
                mstore(32, authorizer)\n\
                mstore(64, authorized)\n\
                mstore(96, newIsAuthorized)\n\
                mstore(128, expectedNonce)\n\
                mstore(160, deadline)\n\
                let hashStruct := keccak256(0, 192)\n\
                mstore(0, 0x1901000000000000000000000000000000000000000000000000000000000000)\n\
                mstore(32, 0)\n\
                mstore(34, hashStruct)\n\
                let digest := keccak256(0, 66)\n\
                mstore(0, digest)\n\
                mstore(32, v)\n\
                mstore(64, r)\n\
                mstore(96, s)\n\
                if iszero(staticcall(gas(), 1, 0, 128, 0, 32)) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 17)\n\
                    mstore(68, 0x696e76616c6964207369676e6174757265000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                let signatory := and(mload(0), 0xffffffffffffffffffffffffffffffffffffffff)\n\
                if iszero(and(iszero(iszero(signatory)), eq(signatory, authorizer))) {\n\
                    mstore(0, 0x8c379a000000000000000000000000000000000000000000000000000000000)\n\
                    mstore(4, 32)\n\
                    mstore(36, 17)\n\
                    mstore(68, 0x696e76616c6964207369676e6174757265000000000000000000000000000000)\n\
                    revert(0, 100)\n\
                }\n\
                mstore(0, expectedNonce)\n\
                log3(0, 32, 0xa58af1a0c70dba0c7aa60d1a1a147ebd61000d1690a968828ac718bca927f2c7, and(caller(), 0xffffffffffffffffffffffffffffffffffffffff), authorizer)\n\
                sstore(mappingSlot(mappingSlot(4, authorizer), authorized), newIsAuthorized)\n\
                mstore(0, newIsAuthorized)\n\
                log4(0, 32, 0xd5e969f01efe921d3f766bdebad25f0a05e3f237311f56482bf132d0326309c0, and(caller(), 0xffffffffffffffffffffffffffffffffffffffff), authorizer, authorized)\n\
                stop()\n\
            }\n"

private def injectSupplyShim (text : String) : Except String String := do
  let needle := "            default {\n                revert(0, 0)\n            }\n"
  let patched := text.replace needle (supplyCase ++ withdrawCase ++ supplyCollateralCase ++ withdrawCollateralCase ++ borrowCase ++ repayCase ++ flashLoanCase ++ setAuthorizationWithSigCase ++ needle)
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

  -- IMorpho defines CreateMarket as `CreateMarket(bytes32,(address,address,address,address,uint256))`.
  -- The current IR event renderer flattens tuple components, so patch topic0 to the canonical tuple hash.
  let createMarketTopicOld :=
    "mstore(add(__evt_ptr, 0), 0x4372656174654d61726b657428627974657333322c616464726573732c616464)\n                    mstore(add(__evt_ptr, 32), 0x726573732c616464726573732c616464726573732c75696e7432353629000000)\n                    let __evt_topic0 := keccak256(__evt_ptr, 61)\n"
  let createMarketTopicNew :=
    "let __evt_topic0 := 0xac4b2400f169220b0c0afdde7a0b32e775ba727ea1cb30b35f935cdaab8683ac\n"
  let t3a ← replaceOrThrow t3 createMarketTopicOld createMarketTopicNew "CreateMarket tuple event topic compatibility"

  let supplyOld := "sstore(mappingSlot(9, id), newTotalSupplyShares)\nmstore(0, assetsSupplied)\n"
  let supplySlot0Packed := packMarketSupplySlot0Expr "newTotalSupplyAssets" "newTotalSupplyShares"
  let supplyNew := s!"sstore(mappingSlot(9, id), newTotalSupplyShares)\nlet __marketSlot0 := mappingSlot(3, id)\nsstore(__marketSlot0, {supplySlot0Packed})\nmstore(0, assetsSupplied)\n"
  let t4 ← replaceOrThrow t3a supplyOld supplyNew "supply packed slot compatibility"

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
