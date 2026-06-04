import Contracts.Common
import Compiler.Modules.Callbacks
import Compiler.Modules.Calls
import Compiler.Modules.Create2SSTORE2
import Compiler.Modules.Oracle
import Verity.Core
import Verity.Macro
import Verity.Stdlib.Math

namespace Midnight.Contract

set_option linter.unusedVariables false
set_option maxRecDepth 10000

open Verity hiding pure bind
open Verity.EVM.Uint256
open Verity.Stdlib.Math
open Compiler.Yul
open Contracts (emit)

def multicallDelegateModule : Compiler.ECM.ExternalCallModule where
  name := "midnightMulticallDelegate"
  numArgs := 0
  resultVars := []
  writesState := true
  readsState := true
  axioms := ["midnight_multicall_bytes_array_abi", "self_delegatecall_storage_context"]
  compile := fun _ctx args =>
    match args with
    | [] =>
        pure [
          YulStmt.block [
            YulStmt.let_ "__mc_head" (YulExpr.call "calldataload" [YulExpr.lit 4]),
            YulStmt.let_ "__mc_base" (YulExpr.call "add" [YulExpr.lit 4, YulExpr.ident "__mc_head"]),
            YulStmt.let_ "__mc_len" (YulExpr.call "calldataload" [YulExpr.ident "__mc_base"]),
            YulStmt.let_ "__mc_i" (YulExpr.lit 0),
            YulStmt.for_
              []
              (YulExpr.call "lt" [YulExpr.ident "__mc_i", YulExpr.ident "__mc_len"])
              [YulStmt.assign "__mc_i" (YulExpr.call "add" [YulExpr.ident "__mc_i", YulExpr.lit 1])]
              [
                YulStmt.let_ "__mc_elem_head" (YulExpr.call "calldataload" [
                  YulExpr.call "add" [
                    YulExpr.call "add" [YulExpr.ident "__mc_base", YulExpr.lit 32],
                    YulExpr.call "mul" [YulExpr.ident "__mc_i", YulExpr.lit 32]
                  ]
                ]),
                YulStmt.let_ "__mc_elem" (YulExpr.call "add" [
                  YulExpr.call "add" [YulExpr.ident "__mc_base", YulExpr.lit 32],
                  YulExpr.ident "__mc_elem_head"
                ]),
                YulStmt.let_ "__mc_size" (YulExpr.call "calldataload" [YulExpr.ident "__mc_elem"]),
                YulStmt.let_ "__mc_data" (YulExpr.call "add" [YulExpr.ident "__mc_elem", YulExpr.lit 32]),
                YulStmt.let_ "__mc_ptr" (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
                YulStmt.expr (YulExpr.call "calldatacopy" [
                  YulExpr.ident "__mc_ptr",
                  YulExpr.ident "__mc_data",
                  YulExpr.ident "__mc_size"
                ]),
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
                  YulExpr.call "add" [
                    YulExpr.ident "__mc_ptr",
                    YulExpr.call "and" [
                      YulExpr.call "add" [YulExpr.ident "__mc_size", YulExpr.lit 31],
                      YulExpr.call "not" [YulExpr.lit 31]
                    ]
                  ]
                ]),
                YulStmt.let_ "__mc_success" (YulExpr.call "delegatecall" [
                  YulExpr.call "gas" [],
                  YulExpr.call "address" [],
                  YulExpr.ident "__mc_ptr",
                  YulExpr.ident "__mc_size",
                  YulExpr.lit 0,
                  YulExpr.lit 0
                ]),
                YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident "__mc_success"]) [
                  YulStmt.let_ "__mc_rds" (YulExpr.call "returndatasize" []),
                  YulStmt.expr (YulExpr.call "returndatacopy" [
                    YulExpr.lit 0, YulExpr.lit 0, YulExpr.ident "__mc_rds"
                  ]),
                  YulStmt.expr (YulExpr.call "revert" [
                    YulExpr.lit 0, YulExpr.ident "__mc_rds"
                  ])
                ]
              ]
          ]
        ]
    | _ =>
        throw s!"midnightMulticallDelegate expects 0 arguments, got {args.length}"

def tickToPriceModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightTickToPrice"
  numArgs := 1
  resultVars := [resultVar]
  writesState := false
  readsState := false
  axioms := ["midnight_ticklib_tick_to_price_exact_yul"]
  compile := fun _ctx args =>
    match args with
    | [tick] =>
        let lnOnePlusDelta := YulExpr.lit 4987541511039073
        let maxTickHalf := YulExpr.lit 2910
        let ln2 := YulExpr.lit 693147180559945309
        let offset := YulExpr.lit 322611214989459870
        let oneE18 := YulExpr.lit 1000000000000000000
        let oneE36 := YulExpr.lit 1000000000000000000000000000000000000
        let priceRoundingStep := YulExpr.lit 1000000000000
        pure [
          YulStmt.let_ resultVar (YulExpr.lit 0),
          YulStmt.block [
            YulStmt.let_ "__tp_x" (YulExpr.call "mul" [
              lnOnePlusDelta,
              YulExpr.call "sub" [maxTickHalf, tick]
            ]),
            YulStmt.let_ "__tp_abs" (YulExpr.ident "__tp_x"),
            YulStmt.let_ "__tp_negative" (YulExpr.call "slt" [YulExpr.ident "__tp_x", YulExpr.lit 0]),
            YulStmt.if_ (YulExpr.ident "__tp_negative") [
              YulStmt.assign "__tp_abs" (YulExpr.call "sub" [YulExpr.lit 0, YulExpr.ident "__tp_x"])
            ],
            YulStmt.let_ "__tp_q" (YulExpr.call "div" [
              YulExpr.call "add" [YulExpr.ident "__tp_abs", offset],
              ln2
            ]),
            YulStmt.let_ "__tp_r" (YulExpr.call "sub" [
              YulExpr.ident "__tp_abs",
              YulExpr.call "mul" [YulExpr.ident "__tp_q", ln2]
            ]),
            YulStmt.let_ "__tp_second" (YulExpr.call "sdiv" [
              YulExpr.call "mul" [YulExpr.ident "__tp_r", YulExpr.ident "__tp_r"],
              YulExpr.lit 2000000000000000000
            ]),
            YulStmt.let_ "__tp_third" (YulExpr.call "sdiv" [
              YulExpr.call "mul" [YulExpr.ident "__tp_second", YulExpr.ident "__tp_r"],
              YulExpr.lit 3000000000000000000
            ]),
            YulStmt.let_ "__tp_expR" (YulExpr.call "add" [
              oneE18,
              YulExpr.call "add" [
                YulExpr.ident "__tp_r",
                YulExpr.call "add" [YulExpr.ident "__tp_second", YulExpr.ident "__tp_third"]
              ]
            ]),
            YulStmt.let_ "__tp_wexp" (YulExpr.call "shl" [
              YulExpr.ident "__tp_q", YulExpr.ident "__tp_expR"
            ]),
            YulStmt.if_ (YulExpr.ident "__tp_negative") [
              YulStmt.assign "__tp_wexp" (YulExpr.call "div" [oneE36, YulExpr.ident "__tp_wexp"])
            ],
            YulStmt.let_ "__tp_den" (YulExpr.call "add" [oneE18, YulExpr.ident "__tp_wexp"]),
            YulStmt.let_ "__tp_raw" (YulExpr.call "div" [
              YulExpr.call "add" [
                oneE36,
                YulExpr.call "div" [YulExpr.call "sub" [YulExpr.ident "__tp_den", YulExpr.lit 1], YulExpr.lit 2]
              ],
              YulExpr.ident "__tp_den"
            ]),
            YulStmt.assign resultVar (YulExpr.call "mul" [
              YulExpr.call "div" [
                YulExpr.call "add" [
                  YulExpr.ident "__tp_raw",
                  YulExpr.call "div" [YulExpr.call "sub" [priceRoundingStep, YulExpr.lit 1], YulExpr.lit 2]
                ],
                priceRoundingStep
              ],
              priceRoundingStep
            ])
          ]
        ]
    | _ =>
        throw s!"midnightTickToPrice expects 1 argument, got {args.length}"

def flashLoanCallbackModule : Compiler.ECM.ExternalCallModule where
  name := "midnightFlashLoanCallback"
  numArgs := 2
  resultVars := []
  writesState := true
  readsState := false
  axioms := ["midnight_flashloan_callback_dynamic_abi"]
  compile := fun _ctx args =>
    match args with
    | [target, caller] =>
        let ptr := YulExpr.ident "__flcb_ptr"
        let paddedDataLen := YulExpr.ident "__flcb_padded_data_len"
        let tokensBytes := YulExpr.ident "__flcb_tokens_bytes"
        let assetsBytes := YulExpr.ident "__flcb_assets_bytes"
        let dataLen := YulExpr.ident "data_length"
        pure [
          YulStmt.block [
            YulStmt.let_ "__flcb_ptr" (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.let_ "__flcb_tokens_bytes" (YulExpr.call "mul" [YulExpr.ident "tokens_length", YulExpr.lit 32]),
            YulStmt.let_ "__flcb_assets_bytes" (YulExpr.call "mul" [YulExpr.ident "assets_length", YulExpr.lit 32]),
            YulStmt.let_ "__flcb_tokens_seg" (YulExpr.call "add" [YulExpr.lit 32, tokensBytes]),
            YulStmt.let_ "__flcb_assets_seg" (YulExpr.call "add" [YulExpr.lit 32, assetsBytes]),
            YulStmt.let_ "__flcb_data_off" (YulExpr.call "add" [
              YulExpr.lit 128,
              YulExpr.call "add" [YulExpr.ident "__flcb_tokens_seg", YulExpr.ident "__flcb_assets_seg"]
            ]),
            YulStmt.let_ "__flcb_tokens_pos" (YulExpr.call "add" [ptr, YulExpr.lit 132]),
            YulStmt.let_ "__flcb_assets_pos" (YulExpr.call "add" [
              ptr,
              YulExpr.call "add" [YulExpr.lit 132, YulExpr.ident "__flcb_tokens_seg"]
            ]),
            YulStmt.let_ "__flcb_data_pos" (YulExpr.call "add" [
              ptr,
              YulExpr.call "add" [YulExpr.lit 4, YulExpr.ident "__flcb_data_off"]
            ]),
            YulStmt.let_ "__flcb_padded_data_len" (YulExpr.call "and" [
              YulExpr.call "add" [dataLen, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__flcb_total" (YulExpr.call "add" [
              YulExpr.call "add" [YulExpr.ident "__flcb_data_pos", YulExpr.lit 32],
              paddedDataLen
            ]),
            YulStmt.assign "__flcb_total" (YulExpr.call "sub" [YulExpr.ident "__flcb_total", ptr]),
            YulStmt.expr (YulExpr.call "mstore" [ptr, YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0xd1f260c3]]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 4], caller]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 36], YulExpr.lit 128]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [ptr, YulExpr.lit 68],
              YulExpr.call "add" [YulExpr.lit 128, YulExpr.ident "__flcb_tokens_seg"]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 100], YulExpr.ident "__flcb_data_off"]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__flcb_tokens_pos", YulExpr.ident "tokens_length"]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [YulExpr.ident "__flcb_tokens_pos", YulExpr.lit 32],
              YulExpr.ident "tokens_data_offset",
              tokensBytes
            ]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__flcb_assets_pos", YulExpr.ident "assets_length"]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [YulExpr.ident "__flcb_assets_pos", YulExpr.lit 32],
              YulExpr.ident "assets_data_offset",
              assetsBytes
            ]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__flcb_data_pos", dataLen]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [YulExpr.ident "__flcb_data_pos", YulExpr.lit 32],
              YulExpr.ident "data_data_offset",
              dataLen
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [ptr, YulExpr.call "and" [
                YulExpr.call "add" [YulExpr.ident "__flcb_total", YulExpr.lit 31],
                YulExpr.call "not" [YulExpr.lit 31]
              ]]
            ]),
            YulStmt.let_ "__flcb_success" (YulExpr.call "call" [
              YulExpr.call "gas" [], target, YulExpr.lit 0, ptr, YulExpr.ident "__flcb_total", ptr, YulExpr.lit 32
            ]),
            YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident "__flcb_success"]) [
              YulStmt.let_ "__flcb_rds" (YulExpr.call "returndatasize" []),
              YulStmt.expr (YulExpr.call "returndatacopy" [YulExpr.lit 0, YulExpr.lit 0, YulExpr.ident "__flcb_rds"]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.ident "__flcb_rds"])
            ],
            YulStmt.if_ (YulExpr.call "lt" [YulExpr.call "returndatasize" [], YulExpr.lit 32]) [
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 0])
            ],
            YulStmt.if_ (YulExpr.call "iszero" [
              YulExpr.call "eq" [YulExpr.call "mload" [ptr], YulExpr.hex 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2]
            ]) [
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 0])
            ]
          ]
        ]
    | _ =>
        throw s!"midnightFlashLoanCallback expects 2 arguments, got {args.length}"

def liquidateCallbackModule : Compiler.ECM.ExternalCallModule where
  name := "midnightLiquidateCallback"
  numArgs := 9
  resultVars := []
  writesState := true
  readsState := false
  axioms := ["midnight_liquidate_callback_market_bytes_dynamic_abi"]
  compile := fun _ctx args =>
    match args with
    | [target, caller, id, collateralIndex, seizedAssets, repaidUnits, borrower, receiver, badDebt] =>
        let ptr := YulExpr.ident "__liqcb_ptr"
        let marketPtr := YulExpr.ident "__liqcb_market_ptr"
        let marketDataOffset := YulExpr.ident "market_data_offset"
        let collateralOffset := YulExpr.ident "__liqcb_collateral_offset"
        let collateralLength := YulExpr.ident "__liqcb_collateral_length"
        let collateralBytes := YulExpr.ident "__liqcb_collateral_bytes"
        let marketSize := YulExpr.ident "__liqcb_market_size"
        let paddedMarketSize := YulExpr.ident "__liqcb_padded_market_size"
        let dataPtr := YulExpr.ident "__liqcb_data_ptr"
        let dataLen := YulExpr.ident "data_length"
        let paddedDataLen := YulExpr.ident "__liqcb_padded_data_len"
        let total := YulExpr.ident "__liqcb_total"
        pure [
          YulStmt.block [
            YulStmt.let_ "__liqcb_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.let_ "__liqcb_collateral_offset" (YulExpr.call "add" [
              marketDataOffset,
              YulExpr.call "calldataload" [
                YulExpr.call "add" [marketDataOffset, YulExpr.lit 32]
              ]
            ]),
            YulStmt.let_ "__liqcb_collateral_length"
              (YulExpr.call "calldataload" [collateralOffset]),
            YulStmt.let_ "__liqcb_collateral_bytes" (YulExpr.call "mul" [
              collateralLength,
              YulExpr.lit 128
            ]),
            YulStmt.let_ "__liqcb_market_size" (YulExpr.call "add" [
              YulExpr.lit 224,
              collateralBytes
            ]),
            YulStmt.let_ "__liqcb_padded_market_size" (YulExpr.call "and" [
              YulExpr.call "add" [marketSize, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__liqcb_padded_data_len" (YulExpr.call "and" [
              YulExpr.call "add" [dataLen, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__liqcb_market_ptr" (YulExpr.call "add" [
              ptr,
              YulExpr.lit 324
            ]),
            YulStmt.let_ "__liqcb_data_ptr" (YulExpr.call "add" [
              marketPtr,
              paddedMarketSize
            ]),
            YulStmt.let_ "__liqcb_total" (YulExpr.call "add" [
              YulExpr.lit 324,
              YulExpr.call "add" [
                paddedMarketSize,
                YulExpr.call "add" [YulExpr.lit 32, paddedDataLen]
              ]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              ptr,
              YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0x6861b795]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 4], caller]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 36], id]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 68], YulExpr.lit 320]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 100], collateralIndex]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 132], seizedAssets]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 164], repaidUnits]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 196], borrower]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 228], receiver]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [ptr, YulExpr.lit 260],
              YulExpr.call "add" [YulExpr.lit 320, paddedMarketSize]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 292], badDebt]),
            YulStmt.expr (YulExpr.call "mstore" [marketPtr, YulExpr.call "calldataload" [marketDataOffset]]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [marketPtr, YulExpr.lit 32], YulExpr.lit 192]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 64],
              YulExpr.call "calldataload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 64]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 96],
              YulExpr.call "calldataload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 96]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 128],
              YulExpr.call "calldataload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 128]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 160],
              YulExpr.call "calldataload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 160]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 192],
              collateralLength
            ]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 224],
              YulExpr.call "add" [collateralOffset, YulExpr.lit 32],
              collateralBytes
            ]),
            YulStmt.expr (YulExpr.call "mstore" [dataPtr, dataLen]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [dataPtr, YulExpr.lit 32],
              YulExpr.ident "data_data_offset",
              dataLen
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [
                ptr,
                YulExpr.call "and" [
                  YulExpr.call "add" [total, YulExpr.lit 31],
                  YulExpr.call "not" [YulExpr.lit 31]
                ]
              ]
            ]),
            YulStmt.let_ "__liqcb_success" (YulExpr.call "call" [
              YulExpr.call "gas" [], target, YulExpr.lit 0, ptr, total, ptr, YulExpr.lit 32
            ]),
            YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident "__liqcb_success"]) [
              YulStmt.let_ "__liqcb_rds" (YulExpr.call "returndatasize" []),
              YulStmt.expr (YulExpr.call "returndatacopy" [
                YulExpr.lit 0, YulExpr.lit 0, YulExpr.ident "__liqcb_rds"
              ]),
              YulStmt.expr (YulExpr.call "revert" [
                YulExpr.lit 0, YulExpr.ident "__liqcb_rds"
              ])
            ],
            YulStmt.if_ (YulExpr.call "lt" [YulExpr.call "returndatasize" [], YulExpr.lit 32]) [
              YulStmt.expr (YulExpr.call "mstore" [
                YulExpr.lit 0,
                YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0x70b53d4b]
              ]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 4])
            ],
            YulStmt.if_ (YulExpr.call "iszero" [
              YulExpr.call "eq" [
                YulExpr.call "mload" [ptr],
                YulExpr.hex 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2
              ]
            ]) [
              YulStmt.expr (YulExpr.call "mstore" [
                YulExpr.lit 0,
                YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0x70b53d4b]
              ]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 4])
            ]
          ]
        ]
    | _ =>
        throw s!"midnightLiquidateCallback expects 9 arguments, got {args.length}"

def ratifierCallbackModule : Compiler.ECM.ExternalCallModule where
  name := "midnightRatifierCallback"
  numArgs := 1
  resultVars := []
  writesState := false
  readsState := false
  axioms := ["midnight_ratifier_offer_bytes_dynamic_abi"]
  compile := fun _ctx args =>
    match args with
    | [target] =>
        let ptr := YulExpr.ident "__rat_ptr"
        let offerPtr := YulExpr.ident "__rat_offer_ptr"
        let offerOffset := YulExpr.ident "offer_data_offset"
        let marketPtr := YulExpr.ident "__rat_market_ptr"
        let marketOffset := YulExpr.ident "__rat_market_offset"
        let collateralOffset := YulExpr.ident "__rat_collateral_offset"
        let collateralLength := YulExpr.ident "__rat_collateral_length"
        let collateralBytes := YulExpr.ident "__rat_collateral_bytes"
        let marketSize := YulExpr.ident "__rat_market_size"
        let paddedMarketSize := YulExpr.ident "__rat_padded_market_size"
        let callbackDataOffset := YulExpr.ident "__rat_callback_data_offset"
        let callbackDataLength := YulExpr.ident "__rat_callback_data_length"
        let paddedCallbackDataLength := YulExpr.ident "__rat_padded_callback_data_length"
        let offerSize := YulExpr.ident "__rat_offer_size"
        let paddedOfferSize := YulExpr.ident "__rat_padded_offer_size"
        let dataPtr := YulExpr.ident "__rat_data_ptr"
        let ratifierDataLength := YulExpr.ident "ratifierData_length"
        let paddedRatifierDataLength := YulExpr.ident "__rat_padded_data_length"
        let total := YulExpr.ident "__rat_total"
        pure [
          YulStmt.block [
            YulStmt.let_ "__rat_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.let_ "__rat_offer_ptr" (YulExpr.call "add" [ptr, YulExpr.lit 68]),
            YulStmt.let_ "__rat_market_offset"
              (YulExpr.call "add" [offerOffset, YulExpr.call "calldataload" [offerOffset]]),
            YulStmt.let_ "__rat_collateral_offset" (YulExpr.call "add" [
              marketOffset,
              YulExpr.call "calldataload" [YulExpr.call "add" [marketOffset, YulExpr.lit 32]]
            ]),
            YulStmt.let_ "__rat_collateral_length"
              (YulExpr.call "calldataload" [collateralOffset]),
            YulStmt.let_ "__rat_collateral_bytes" (YulExpr.call "mul" [
              collateralLength, YulExpr.lit 128
            ]),
            YulStmt.let_ "__rat_market_size" (YulExpr.call "add" [
              YulExpr.lit 224, collateralBytes
            ]),
            YulStmt.let_ "__rat_padded_market_size" (YulExpr.call "and" [
              YulExpr.call "add" [marketSize, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__rat_callback_data_offset" (YulExpr.call "add" [
              offerOffset,
              YulExpr.call "calldataload" [YulExpr.call "add" [offerOffset, YulExpr.lit 256]]
            ]),
            YulStmt.let_ "__rat_callback_data_length"
              (YulExpr.call "calldataload" [callbackDataOffset]),
            YulStmt.let_ "__rat_padded_callback_data_length" (YulExpr.call "and" [
              YulExpr.call "add" [callbackDataLength, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__rat_offer_size" (YulExpr.call "add" [
              YulExpr.lit 448,
              YulExpr.call "add" [
                paddedMarketSize,
                YulExpr.call "add" [YulExpr.lit 32, paddedCallbackDataLength]
              ]
            ]),
            YulStmt.let_ "__rat_padded_offer_size" (YulExpr.call "and" [
              YulExpr.call "add" [offerSize, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__rat_data_ptr"
              (YulExpr.call "add" [offerPtr, paddedOfferSize]),
            YulStmt.let_ "__rat_padded_data_length" (YulExpr.call "and" [
              YulExpr.call "add" [ratifierDataLength, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__rat_total" (YulExpr.call "add" [
              YulExpr.lit 68,
              YulExpr.call "add" [
                paddedOfferSize,
                YulExpr.call "add" [YulExpr.lit 32, paddedRatifierDataLength]
              ]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              ptr,
              YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0x675ef8d3]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 4], YulExpr.lit 64]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [ptr, YulExpr.lit 36],
              YulExpr.call "add" [YulExpr.lit 64, paddedOfferSize]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [offerPtr, YulExpr.lit 448]),
            YulStmt.for_
              [YulStmt.let_ "__rat_i" (YulExpr.lit 1)]
              (YulExpr.call "lt" [YulExpr.ident "__rat_i", YulExpr.lit 8])
              [YulStmt.assign "__rat_i" (YulExpr.call "add" [YulExpr.ident "__rat_i", YulExpr.lit 1])]
              [
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [offerPtr, YulExpr.call "mul" [YulExpr.ident "__rat_i", YulExpr.lit 32]],
                  YulExpr.call "calldataload" [
                    YulExpr.call "add" [offerOffset, YulExpr.call "mul" [YulExpr.ident "__rat_i", YulExpr.lit 32]]
                  ]
                ])
              ],
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [offerPtr, YulExpr.lit 256],
              YulExpr.call "add" [YulExpr.lit 448, paddedMarketSize]
            ]),
            YulStmt.for_
              [YulStmt.let_ "__rat_j" (YulExpr.lit 9)]
              (YulExpr.call "lt" [YulExpr.ident "__rat_j", YulExpr.lit 14])
              [YulStmt.assign "__rat_j" (YulExpr.call "add" [YulExpr.ident "__rat_j", YulExpr.lit 1])]
              [
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [offerPtr, YulExpr.call "mul" [YulExpr.ident "__rat_j", YulExpr.lit 32]],
                  YulExpr.call "calldataload" [
                    YulExpr.call "add" [offerOffset, YulExpr.call "mul" [YulExpr.ident "__rat_j", YulExpr.lit 32]]
                  ]
                ])
              ],
            YulStmt.let_ "__rat_market_ptr" (YulExpr.call "add" [offerPtr, YulExpr.lit 448]),
            YulStmt.expr (YulExpr.call "mstore" [marketPtr, YulExpr.call "calldataload" [marketOffset]]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [marketPtr, YulExpr.lit 32], YulExpr.lit 192]),
            YulStmt.for_
              [YulStmt.let_ "__rat_m" (YulExpr.lit 2)]
              (YulExpr.call "lt" [YulExpr.ident "__rat_m", YulExpr.lit 6])
              [YulStmt.assign "__rat_m" (YulExpr.call "add" [YulExpr.ident "__rat_m", YulExpr.lit 1])]
              [
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [marketPtr, YulExpr.call "mul" [YulExpr.ident "__rat_m", YulExpr.lit 32]],
                  YulExpr.call "calldataload" [
                    YulExpr.call "add" [marketOffset, YulExpr.call "mul" [YulExpr.ident "__rat_m", YulExpr.lit 32]]
                  ]
                ])
              ],
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [marketPtr, YulExpr.lit 192], collateralLength]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 224],
              YulExpr.call "add" [collateralOffset, YulExpr.lit 32],
              collateralBytes
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [offerPtr, YulExpr.call "add" [YulExpr.lit 448, paddedMarketSize]],
              callbackDataLength
            ]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [
                offerPtr,
                YulExpr.call "add" [YulExpr.lit 480, paddedMarketSize]
              ],
              YulExpr.call "add" [callbackDataOffset, YulExpr.lit 32],
              callbackDataLength
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [
                offerPtr,
                YulExpr.call "add" [
                  YulExpr.lit 480,
                  YulExpr.call "add" [paddedMarketSize, callbackDataLength]
                ]
              ],
              YulExpr.lit 0
            ]),
            YulStmt.expr (YulExpr.call "mstore" [dataPtr, ratifierDataLength]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [dataPtr, YulExpr.lit 32],
              YulExpr.ident "ratifierData_data_offset",
              ratifierDataLength
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [dataPtr, YulExpr.call "add" [YulExpr.lit 32, ratifierDataLength]],
              YulExpr.lit 0
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [ptr, YulExpr.call "and" [
                YulExpr.call "add" [total, YulExpr.lit 31],
                YulExpr.call "not" [YulExpr.lit 31]
              ]]
            ]),
            YulStmt.let_ "__rat_success" (YulExpr.call "staticcall" [
              YulExpr.call "gas" [], target, ptr, total, ptr, YulExpr.lit 32
            ]),
            YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident "__rat_success"]) [
              YulStmt.let_ "__rat_rds" (YulExpr.call "returndatasize" []),
              YulStmt.expr (YulExpr.call "returndatacopy" [YulExpr.lit 0, YulExpr.lit 0, YulExpr.ident "__rat_rds"]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.ident "__rat_rds"])
            ],
            YulStmt.if_ (YulExpr.call "iszero" [
              YulExpr.call "and" [
                YulExpr.call "iszero" [YulExpr.call "lt" [YulExpr.call "returndatasize" [], YulExpr.lit 32]],
                YulExpr.call "eq" [
                  YulExpr.call "mload" [ptr],
                  YulExpr.hex 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2
                ]
              ]
            ]) [
              YulStmt.expr (YulExpr.call "mstore" [
                YulExpr.lit 0,
                YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0x9e8ec676]
              ]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 4])
            ]
          ]
        ]
    | _ =>
        throw s!"midnightRatifierCallback expects 1 argument, got {args.length}"

def buyCallbackModule (useOfferData : Bool) : Compiler.ECM.ExternalCallModule where
  name := if useOfferData then "midnightBuyOfferCallback" else "midnightBuyTakerCallback"
  numArgs := 6
  resultVars := []
  writesState := true
  readsState := false
  axioms := ["midnight_buy_callback_market_bytes_dynamic_abi"]
  compile := fun _ctx args =>
    match args with
    | [target, id, buyerAssets, units, pendingFeeIncrease, buyer] =>
        let ptr := YulExpr.ident "__buycb_ptr"
        let marketPtr := YulExpr.ident "__buycb_market_ptr"
        let offerOffset := YulExpr.ident "offer_data_offset"
        let marketOffset := YulExpr.ident "__buycb_market_offset"
        let collateralOffset := YulExpr.ident "__buycb_collateral_offset"
        let collateralLength := YulExpr.ident "__buycb_collateral_length"
        let collateralBytes := YulExpr.ident "__buycb_collateral_bytes"
        let marketSize := YulExpr.ident "__buycb_market_size"
        let paddedMarketSize := YulExpr.ident "__buycb_padded_market_size"
        let dataOffset := YulExpr.ident "__buycb_data_offset"
        let dataLen := YulExpr.ident "__buycb_data_length"
        let paddedDataLen := YulExpr.ident "__buycb_padded_data_len"
        let dataPtr := YulExpr.ident "__buycb_data_ptr"
        let total := YulExpr.ident "__buycb_total"
        let initDataOffset :=
          if useOfferData then
            YulStmt.let_ "__buycb_data_offset" (YulExpr.call "add" [
              offerOffset,
              YulExpr.call "calldataload" [YulExpr.call "add" [offerOffset, YulExpr.lit 256]]
            ])
          else
            YulStmt.let_ "__buycb_data_offset"
              (YulExpr.call "sub" [YulExpr.ident "takerCallbackData_data_offset", YulExpr.lit 32])
        pure [
          YulStmt.block [
            YulStmt.let_ "__buycb_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.let_ "__buycb_market_offset"
              (YulExpr.call "add" [offerOffset, YulExpr.call "calldataload" [offerOffset]]),
            YulStmt.let_ "__buycb_collateral_offset" (YulExpr.call "add" [
              marketOffset,
              YulExpr.call "calldataload" [YulExpr.call "add" [marketOffset, YulExpr.lit 32]]
            ]),
            YulStmt.let_ "__buycb_collateral_length"
              (YulExpr.call "calldataload" [collateralOffset]),
            YulStmt.let_ "__buycb_collateral_bytes"
              (YulExpr.call "mul" [collateralLength, YulExpr.lit 128]),
            YulStmt.let_ "__buycb_market_size" (YulExpr.call "add" [
              YulExpr.lit 224,
              collateralBytes
            ]),
            YulStmt.let_ "__buycb_padded_market_size" (YulExpr.call "and" [
              YulExpr.call "add" [marketSize, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            initDataOffset,
            YulStmt.let_ "__buycb_data_length" (YulExpr.call "calldataload" [dataOffset]),
            YulStmt.let_ "__buycb_padded_data_len" (YulExpr.call "and" [
              YulExpr.call "add" [dataLen, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__buycb_market_ptr" (YulExpr.call "add" [ptr, YulExpr.lit 228]),
            YulStmt.let_ "__buycb_data_ptr" (YulExpr.call "add" [marketPtr, paddedMarketSize]),
            YulStmt.let_ "__buycb_total" (YulExpr.call "add" [
              YulExpr.lit 228,
              YulExpr.call "add" [
                paddedMarketSize,
                YulExpr.call "add" [YulExpr.lit 32, paddedDataLen]
              ]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              ptr,
              YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0xf151bd5c]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 4], id]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 36], YulExpr.lit 224]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 68], buyerAssets]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 100], units]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 132], pendingFeeIncrease]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 164], buyer]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [ptr, YulExpr.lit 196],
              YulExpr.call "add" [YulExpr.lit 224, paddedMarketSize]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [marketPtr, YulExpr.call "calldataload" [marketOffset]]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [marketPtr, YulExpr.lit 32], YulExpr.lit 192]),
            YulStmt.for_
              [YulStmt.let_ "__buycb_m" (YulExpr.lit 2)]
              (YulExpr.call "lt" [YulExpr.ident "__buycb_m", YulExpr.lit 6])
              [YulStmt.assign "__buycb_m" (YulExpr.call "add" [YulExpr.ident "__buycb_m", YulExpr.lit 1])]
              [
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [marketPtr, YulExpr.call "mul" [YulExpr.ident "__buycb_m", YulExpr.lit 32]],
                  YulExpr.call "calldataload" [
                    YulExpr.call "add" [marketOffset, YulExpr.call "mul" [YulExpr.ident "__buycb_m", YulExpr.lit 32]]
                  ]
                ])
              ],
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [marketPtr, YulExpr.lit 192], collateralLength]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 224],
              YulExpr.call "add" [collateralOffset, YulExpr.lit 32],
              collateralBytes
            ]),
            YulStmt.expr (YulExpr.call "mstore" [dataPtr, dataLen]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [dataPtr, YulExpr.lit 32],
              YulExpr.call "add" [dataOffset, YulExpr.lit 32],
              dataLen
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [dataPtr, YulExpr.call "add" [YulExpr.lit 32, dataLen]],
              YulExpr.lit 0
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [ptr, YulExpr.call "and" [
                YulExpr.call "add" [total, YulExpr.lit 31],
                YulExpr.call "not" [YulExpr.lit 31]
              ]]
            ]),
            YulStmt.let_ "__buycb_success" (YulExpr.call "call" [
              YulExpr.call "gas" [], target, YulExpr.lit 0, ptr, total, ptr, YulExpr.lit 32
            ]),
            YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident "__buycb_success"]) [
              YulStmt.let_ "__buycb_rds" (YulExpr.call "returndatasize" []),
              YulStmt.expr (YulExpr.call "returndatacopy" [YulExpr.lit 0, YulExpr.lit 0, YulExpr.ident "__buycb_rds"]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.ident "__buycb_rds"])
            ],
            YulStmt.if_ (YulExpr.call "iszero" [
              YulExpr.call "and" [
                YulExpr.call "iszero" [YulExpr.call "lt" [YulExpr.call "returndatasize" [], YulExpr.lit 32]],
                YulExpr.call "eq" [
                  YulExpr.call "mload" [ptr],
                  YulExpr.hex 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2
                ]
              ]
            ]) [
              YulStmt.expr (YulExpr.call "mstore" [
                YulExpr.lit 0,
                YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0xa8f3eb44]
              ]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 4])
            ]
          ]
        ]
    | _ =>
        throw s!"midnightBuyCallback expects 6 arguments, got {args.length}"

def repayCallbackModule : Compiler.ECM.ExternalCallModule where
  name := "midnightRepayCallback"
  numArgs := 4
  resultVars := []
  writesState := true
  readsState := false
  axioms := ["midnight_repay_callback_market_bytes_dynamic_abi"]
  compile := fun _ctx args =>
    match args with
    | [target, id, units, onBehalf] =>
        let ptr := YulExpr.ident "__repaycb_ptr"
        let marketPtr := YulExpr.ident "__repaycb_market_ptr"
        let marketOffset := YulExpr.ident "__repaycb_market_offset"
        let collateralOffset := YulExpr.ident "__repaycb_collateral_offset"
        let collateralLength := YulExpr.ident "__repaycb_collateral_length"
        let collateralBytes := YulExpr.ident "__repaycb_collateral_bytes"
        let marketSize := YulExpr.ident "__repaycb_market_size"
        let paddedMarketSize := YulExpr.ident "__repaycb_padded_market_size"
        let dataOffset := YulExpr.ident "__repaycb_data_offset"
        let dataLen := YulExpr.ident "__repaycb_data_length"
        let paddedDataLen := YulExpr.ident "__repaycb_padded_data_len"
        let dataPtr := YulExpr.ident "__repaycb_data_ptr"
        let total := YulExpr.ident "__repaycb_total"
        pure [
          YulStmt.block [
            YulStmt.let_ "__repaycb_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.let_ "__repaycb_market_offset" (YulExpr.call "add" [
              YulExpr.lit 4,
              YulExpr.call "calldataload" [YulExpr.lit 4]
            ]),
            YulStmt.let_ "__repaycb_collateral_offset" (YulExpr.call "add" [
              marketOffset,
              YulExpr.call "calldataload" [YulExpr.call "add" [marketOffset, YulExpr.lit 32]]
            ]),
            YulStmt.let_ "__repaycb_collateral_length"
              (YulExpr.call "calldataload" [collateralOffset]),
            YulStmt.let_ "__repaycb_collateral_bytes"
              (YulExpr.call "mul" [collateralLength, YulExpr.lit 128]),
            YulStmt.let_ "__repaycb_market_size" (YulExpr.call "add" [
              YulExpr.lit 224,
              collateralBytes
            ]),
            YulStmt.let_ "__repaycb_padded_market_size" (YulExpr.call "and" [
              YulExpr.call "add" [marketSize, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__repaycb_data_offset"
              (YulExpr.call "sub" [YulExpr.ident "data_data_offset", YulExpr.lit 32]),
            YulStmt.let_ "__repaycb_data_length" (YulExpr.call "calldataload" [dataOffset]),
            YulStmt.let_ "__repaycb_padded_data_len" (YulExpr.call "and" [
              YulExpr.call "add" [dataLen, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__repaycb_market_ptr" (YulExpr.call "add" [ptr, YulExpr.lit 164]),
            YulStmt.let_ "__repaycb_data_ptr" (YulExpr.call "add" [marketPtr, paddedMarketSize]),
            YulStmt.let_ "__repaycb_total" (YulExpr.call "add" [
              YulExpr.lit 164,
              YulExpr.call "add" [
                paddedMarketSize,
                YulExpr.call "add" [YulExpr.lit 32, paddedDataLen]
              ]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              ptr,
              YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0xfc56f72e]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 4], id]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 36], YulExpr.lit 160]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 68], units]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 100], onBehalf]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [ptr, YulExpr.lit 132],
              YulExpr.call "add" [YulExpr.lit 160, paddedMarketSize]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [marketPtr, YulExpr.call "calldataload" [marketOffset]]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [marketPtr, YulExpr.lit 32], YulExpr.lit 192]),
            YulStmt.for_
              [YulStmt.let_ "__repaycb_m" (YulExpr.lit 2)]
              (YulExpr.call "lt" [YulExpr.ident "__repaycb_m", YulExpr.lit 6])
              [YulStmt.assign "__repaycb_m" (YulExpr.call "add" [YulExpr.ident "__repaycb_m", YulExpr.lit 1])]
              [
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [marketPtr, YulExpr.call "mul" [YulExpr.ident "__repaycb_m", YulExpr.lit 32]],
                  YulExpr.call "calldataload" [
                    YulExpr.call "add" [marketOffset, YulExpr.call "mul" [YulExpr.ident "__repaycb_m", YulExpr.lit 32]]
                  ]
                ])
              ],
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [marketPtr, YulExpr.lit 192], collateralLength]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 224],
              YulExpr.call "add" [collateralOffset, YulExpr.lit 32],
              collateralBytes
            ]),
            YulStmt.expr (YulExpr.call "mstore" [dataPtr, dataLen]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [dataPtr, YulExpr.lit 32],
              YulExpr.call "add" [dataOffset, YulExpr.lit 32],
              dataLen
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [dataPtr, YulExpr.call "add" [YulExpr.lit 32, dataLen]],
              YulExpr.lit 0
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [ptr, YulExpr.call "and" [
                YulExpr.call "add" [total, YulExpr.lit 31],
                YulExpr.call "not" [YulExpr.lit 31]
              ]]
            ]),
            YulStmt.let_ "__repaycb_success" (YulExpr.call "call" [
              YulExpr.call "gas" [], target, YulExpr.lit 0, ptr, total, ptr, YulExpr.lit 32
            ]),
            YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident "__repaycb_success"]) [
              YulStmt.let_ "__repaycb_rds" (YulExpr.call "returndatasize" []),
              YulStmt.expr (YulExpr.call "returndatacopy" [YulExpr.lit 0, YulExpr.lit 0, YulExpr.ident "__repaycb_rds"]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.ident "__repaycb_rds"])
            ],
            YulStmt.if_ (YulExpr.call "iszero" [
              YulExpr.call "and" [
                YulExpr.call "iszero" [YulExpr.call "lt" [YulExpr.call "returndatasize" [], YulExpr.lit 32]],
                YulExpr.call "eq" [
                  YulExpr.call "mload" [ptr],
                  YulExpr.hex 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2
                ]
              ]
            ]) [
              YulStmt.expr (YulExpr.call "mstore" [
                YulExpr.lit 0,
                YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0x40a13da2]
              ]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 4])
            ]
          ]
        ]
    | _ =>
        throw s!"midnightRepayCallback expects 4 arguments, got {args.length}"

def takeEventPrepareAModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightTakeEventPrepareA"
  numArgs := 6
  resultVars := [resultVar]
  writesState := false
  readsState := false
  axioms := ["midnight_take_event_payload_memory"]
  compile := fun _ctx args =>
    match args with
    | [caller, units, offerIsBuy, group, buyerAssets, sellerAssets] =>
        pure [
          YulStmt.let_ resultVar
            (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
          YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident resultVar, caller]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 32], units
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 64], offerIsBuy
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 96], group
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 128], buyerAssets
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 160], sellerAssets
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
            YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 416]
          ])
        ]
    | _ =>
        throw s!"midnightTakeEventPrepareA expects 6 arguments, got {args.length}"

def takeEventPrepareBModule : Compiler.ECM.ExternalCallModule where
  name := "midnightTakeEventPrepareB"
  numArgs := 6
  resultVars := []
  writesState := false
  readsState := false
  axioms := ["midnight_take_event_payload_memory"]
  compile := fun _ctx args =>
    match args with
    | [ptr, consumed, buyerPendingFeeIncrease, sellerPendingFeeDecrease,
        buyerCreditIncrease, sellerCreditDecrease] =>
        pure [
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [ptr, YulExpr.lit 192], consumed
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [ptr, YulExpr.lit 224], buyerPendingFeeIncrease
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [ptr, YulExpr.lit 256], sellerPendingFeeDecrease
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [ptr, YulExpr.lit 288], buyerCreditIncrease
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [ptr, YulExpr.lit 320], sellerCreditDecrease
          ])
        ]
    | _ =>
        throw s!"midnightTakeEventPrepareB expects 6 arguments, got {args.length}"

def takeEventEmitModule : Compiler.ECM.ExternalCallModule where
  name := "midnightTakeEventEmit"
  numArgs := 6
  resultVars := []
  writesState := false
  readsState := false
  axioms := ["midnight_take_event_log4"]
  compile := fun _ctx args =>
    match args with
    | [ptr, receiver, payer, id, taker, maker] =>
        pure [
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [ptr, YulExpr.lit 352], receiver
          ]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [ptr, YulExpr.lit 384], payer
          ]),
          YulStmt.expr (YulExpr.call "log4" [
            ptr,
            YulExpr.lit 416,
            YulExpr.hex 0x9e0c6d3ffe2895519e5543fe8da6e54858f4c06530d7557d808068b0ecdc9bc3,
            id,
            taker,
            maker
          ])
        ]
    | _ =>
        throw s!"midnightTakeEventEmit expects 6 arguments, got {args.length}"

def takeEventAllocModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightTakeEventAlloc"
  numArgs := 0
  resultVars := [resultVar]
  writesState := false
  readsState := false
  axioms := ["midnight_take_event_payload_memory"]
  compile := fun _ctx args =>
    match args with
    | [] =>
        pure [
          YulStmt.let_ resultVar
            (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
            YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 512]
          ])
        ]
    | _ =>
        throw s!"midnightTakeEventAlloc expects 0 arguments, got {args.length}"

def takeEventStoreModule : Compiler.ECM.ExternalCallModule where
  name := "midnightTakeEventStore"
  numArgs := 3
  resultVars := []
  writesState := false
  readsState := false
  axioms := ["midnight_take_event_payload_memory"]
  compile := fun _ctx args =>
    match args with
    | [ptr, offset, value] =>
        pure [
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.call "add" [ptr, offset],
            value
          ])
        ]
    | _ =>
        throw s!"midnightTakeEventStore expects 3 arguments, got {args.length}"

def takeEventEmitFromMemoryModule : Compiler.ECM.ExternalCallModule where
  name := "midnightTakeEventEmitFromMemory"
  numArgs := 1
  resultVars := []
  writesState := false
  readsState := false
  axioms := ["midnight_take_event_log4"]
  compile := fun _ctx args =>
    match args with
    | [ptr] =>
        pure [
          YulStmt.expr (YulExpr.call "log4" [
            ptr,
            YulExpr.lit 416,
            YulExpr.hex 0x9e0c6d3ffe2895519e5543fe8da6e54858f4c06530d7557d808068b0ecdc9bc3,
            YulExpr.call "mload" [YulExpr.call "add" [ptr, YulExpr.lit 416]],
            YulExpr.call "mload" [YulExpr.call "add" [ptr, YulExpr.lit 448]],
            YulExpr.call "mload" [YulExpr.call "add" [ptr, YulExpr.lit 480]]
          ])
        ]
    | _ =>
        throw s!"midnightTakeEventEmitFromMemory expects 1 argument, got {args.length}"

def sellCallbackModule (useOfferData : Bool) : Compiler.ECM.ExternalCallModule where
  name := if useOfferData then "midnightSellOfferCallback" else "midnightSellTakerCallback"
  numArgs := 8
  resultVars := []
  writesState := true
  readsState := false
  axioms := ["midnight_sell_callback_market_bytes_dynamic_abi"]
  compile := fun _ctx args =>
    match args with
    | [target, id, sellerAssets, units, pendingFeeDecrease, seller, receiver, _unused] =>
        let ptr := YulExpr.ident "__sellcb_ptr"
        let marketPtr := YulExpr.ident "__sellcb_market_ptr"
        let offerOffset := YulExpr.ident "offer_data_offset"
        let marketOffset := YulExpr.ident "__sellcb_market_offset"
        let collateralOffset := YulExpr.ident "__sellcb_collateral_offset"
        let collateralLength := YulExpr.ident "__sellcb_collateral_length"
        let collateralBytes := YulExpr.ident "__sellcb_collateral_bytes"
        let marketSize := YulExpr.ident "__sellcb_market_size"
        let paddedMarketSize := YulExpr.ident "__sellcb_padded_market_size"
        let dataOffset := YulExpr.ident "__sellcb_data_offset"
        let dataLen := YulExpr.ident "__sellcb_data_length"
        let paddedDataLen := YulExpr.ident "__sellcb_padded_data_len"
        let dataPtr := YulExpr.ident "__sellcb_data_ptr"
        let total := YulExpr.ident "__sellcb_total"
        let initDataOffset :=
          if useOfferData then
            YulStmt.let_ "__sellcb_data_offset" (YulExpr.call "add" [
              offerOffset,
              YulExpr.call "calldataload" [YulExpr.call "add" [offerOffset, YulExpr.lit 256]]
            ])
          else
            YulStmt.let_ "__sellcb_data_offset"
              (YulExpr.call "sub" [YulExpr.ident "takerCallbackData_data_offset", YulExpr.lit 32])
        pure [
          YulStmt.block [
            YulStmt.let_ "__sellcb_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.let_ "__sellcb_market_offset"
              (YulExpr.call "add" [offerOffset, YulExpr.call "calldataload" [offerOffset]]),
            YulStmt.let_ "__sellcb_collateral_offset" (YulExpr.call "add" [
              marketOffset,
              YulExpr.call "calldataload" [YulExpr.call "add" [marketOffset, YulExpr.lit 32]]
            ]),
            YulStmt.let_ "__sellcb_collateral_length"
              (YulExpr.call "calldataload" [collateralOffset]),
            YulStmt.let_ "__sellcb_collateral_bytes"
              (YulExpr.call "mul" [collateralLength, YulExpr.lit 128]),
            YulStmt.let_ "__sellcb_market_size" (YulExpr.call "add" [
              YulExpr.lit 224,
              collateralBytes
            ]),
            YulStmt.let_ "__sellcb_padded_market_size" (YulExpr.call "and" [
              YulExpr.call "add" [marketSize, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            initDataOffset,
            YulStmt.let_ "__sellcb_data_length" (YulExpr.call "calldataload" [dataOffset]),
            YulStmt.let_ "__sellcb_padded_data_len" (YulExpr.call "and" [
              YulExpr.call "add" [dataLen, YulExpr.lit 31],
              YulExpr.call "not" [YulExpr.lit 31]
            ]),
            YulStmt.let_ "__sellcb_market_ptr" (YulExpr.call "add" [ptr, YulExpr.lit 260]),
            YulStmt.let_ "__sellcb_data_ptr" (YulExpr.call "add" [marketPtr, paddedMarketSize]),
            YulStmt.let_ "__sellcb_total" (YulExpr.call "add" [
              YulExpr.lit 260,
              YulExpr.call "add" [
                paddedMarketSize,
                YulExpr.call "add" [YulExpr.lit 32, paddedDataLen]
              ]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              ptr,
              YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0x7f44a13a]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 4], id]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 36], YulExpr.lit 256]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 68], sellerAssets]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 100], units]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 132], pendingFeeDecrease]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 164], seller]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [ptr, YulExpr.lit 196], receiver]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [ptr, YulExpr.lit 228],
              YulExpr.call "add" [YulExpr.lit 256, paddedMarketSize]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [marketPtr, YulExpr.call "calldataload" [marketOffset]]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [marketPtr, YulExpr.lit 32], YulExpr.lit 192]),
            YulStmt.for_
              [YulStmt.let_ "__sellcb_m" (YulExpr.lit 2)]
              (YulExpr.call "lt" [YulExpr.ident "__sellcb_m", YulExpr.lit 6])
              [YulStmt.assign "__sellcb_m" (YulExpr.call "add" [YulExpr.ident "__sellcb_m", YulExpr.lit 1])]
              [
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [marketPtr, YulExpr.call "mul" [YulExpr.ident "__sellcb_m", YulExpr.lit 32]],
                  YulExpr.call "calldataload" [
                    YulExpr.call "add" [marketOffset, YulExpr.call "mul" [YulExpr.ident "__sellcb_m", YulExpr.lit 32]]
                  ]
                ])
              ],
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.call "add" [marketPtr, YulExpr.lit 192], collateralLength]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [marketPtr, YulExpr.lit 224],
              YulExpr.call "add" [collateralOffset, YulExpr.lit 32],
              collateralBytes
            ]),
            YulStmt.expr (YulExpr.call "mstore" [dataPtr, dataLen]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [dataPtr, YulExpr.lit 32],
              YulExpr.call "add" [dataOffset, YulExpr.lit 32],
              dataLen
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [dataPtr, YulExpr.call "add" [YulExpr.lit 32, dataLen]],
              YulExpr.lit 0
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [ptr, YulExpr.call "and" [
                YulExpr.call "add" [total, YulExpr.lit 31],
                YulExpr.call "not" [YulExpr.lit 31]
              ]]
            ]),
            YulStmt.let_ "__sellcb_success" (YulExpr.call "call" [
              YulExpr.call "gas" [], target, YulExpr.lit 0, ptr, total, ptr, YulExpr.lit 32
            ]),
            YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident "__sellcb_success"]) [
              YulStmt.let_ "__sellcb_rds" (YulExpr.call "returndatasize" []),
              YulStmt.expr (YulExpr.call "returndatacopy" [YulExpr.lit 0, YulExpr.lit 0, YulExpr.ident "__sellcb_rds"]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.ident "__sellcb_rds"])
            ],
            YulStmt.if_ (YulExpr.call "iszero" [
              YulExpr.call "and" [
                YulExpr.call "iszero" [YulExpr.call "lt" [YulExpr.call "returndatasize" [], YulExpr.lit 32]],
                YulExpr.call "eq" [
                  YulExpr.call "mload" [ptr],
                  YulExpr.hex 0x7f87788ea698181ea4d28d1576d0ba4fc92c0dbe5bf75b43692af2ce91dbaea2
                ]
              ]
            ]) [
              YulStmt.expr (YulExpr.call "mstore" [
                YulExpr.lit 0,
                YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0xa4fb7883]
              ]),
              YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 4])
            ]
          ]
        ]
    | _ =>
        throw s!"midnightSellCallback expects 8 arguments, got {args.length}"

def liquidationLockGetModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightLiquidationLockGet"
  numArgs := 2
  resultVars := [resultVar]
  writesState := false
  readsState := true
  axioms := ["midnight_liquidation_lock_transient_slot"]
  compile := fun _ctx args =>
    match args with
    | [id, user] =>
        pure [
          YulStmt.let_ resultVar (YulExpr.lit 0),
          YulStmt.block [
            YulStmt.let_ "__liq_lock_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__liq_lock_ptr", id]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 32],
              user
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 64],
              YulExpr.hex 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4
            ]),
            YulStmt.assign resultVar
              (YulExpr.call "tload" [
                YulExpr.call "keccak256" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 96]
              ])
          ]
        ]
    | _ => throw s!"midnightLiquidationLockGet expects 2 args, got {args.length}"

def liquidationLockExchangeModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightLiquidationLockExchange"
  numArgs := 3
  resultVars := [resultVar]
  writesState := true
  readsState := true
  axioms := ["midnight_liquidation_lock_transient_slot"]
  compile := fun _ctx args =>
    match args with
    | [id, user, value] =>
        pure [
          YulStmt.let_ resultVar (YulExpr.lit 0),
          YulStmt.block [
            YulStmt.let_ "__liq_lock_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__liq_lock_ptr", id]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 32],
              user
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 64],
              YulExpr.hex 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4
            ]),
            YulStmt.let_ "__liq_lock_slot"
              (YulExpr.call "keccak256" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 96]),
            YulStmt.assign resultVar
              (YulExpr.call "tload" [YulExpr.ident "__liq_lock_slot"]),
            YulStmt.expr (YulExpr.call "tstore" [
              YulExpr.ident "__liq_lock_slot", value
            ])
          ]
        ]
    | _ => throw s!"midnightLiquidationLockExchange expects 3 args, got {args.length}"

def liquidationLockClearIfUnlockedGetModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightLiquidationLockClearIfUnlockedGet"
  numArgs := 3
  resultVars := [resultVar]
  writesState := true
  readsState := true
  axioms := ["midnight_liquidation_lock_transient_slot"]
  compile := fun _ctx args =>
    match args with
    | [id, user, wasLocked] =>
        pure [
          YulStmt.let_ resultVar (YulExpr.lit 0),
          YulStmt.block [
            YulStmt.let_ "__liq_lock_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__liq_lock_ptr", id]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 32],
              user
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 64],
              YulExpr.hex 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4
            ]),
            YulStmt.let_ "__liq_lock_slot"
              (YulExpr.call "keccak256" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 96]),
            YulStmt.if_ (YulExpr.call "iszero" [wasLocked]) [
              YulStmt.expr (YulExpr.call "tstore" [
                YulExpr.ident "__liq_lock_slot", YulExpr.lit 0
              ])
            ],
            YulStmt.assign resultVar
              (YulExpr.call "tload" [YulExpr.ident "__liq_lock_slot"])
          ]
        ]
    | _ => throw s!"midnightLiquidationLockClearIfUnlockedGet expects 3 args, got {args.length}"

def liquidationLockDepthEnterModule : Compiler.ECM.ExternalCallModule where
  name := "midnightLiquidationLockDepthEnter"
  numArgs := 2
  resultVars := []
  writesState := true
  readsState := true
  axioms := ["midnight_liquidation_lock_transient_depth_slot"]
  compile := fun _ctx args =>
    match args with
    | [id, user] =>
        pure [
          YulStmt.block [
            YulStmt.let_ "__liq_lock_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__liq_lock_ptr", id]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 32],
              user
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 64],
              YulExpr.hex 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4
            ]),
            YulStmt.let_ "__liq_lock_slot"
              (YulExpr.call "keccak256" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 96]),
            YulStmt.expr (YulExpr.call "tstore" [
              YulExpr.ident "__liq_lock_slot",
              YulExpr.call "add" [
                YulExpr.call "tload" [YulExpr.ident "__liq_lock_slot"],
                YulExpr.lit 1
              ]
            ])
          ]
        ]
    | _ => throw s!"midnightLiquidationLockDepthEnter expects 2 args, got {args.length}"

def liquidationLockDepthExitGetModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightLiquidationLockDepthExitGet"
  numArgs := 2
  resultVars := [resultVar]
  writesState := true
  readsState := true
  axioms := ["midnight_liquidation_lock_transient_depth_slot"]
  compile := fun _ctx args =>
    match args with
    | [id, user] =>
        pure [
          YulStmt.let_ resultVar (YulExpr.lit 0),
          YulStmt.block [
            YulStmt.let_ "__liq_lock_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [YulExpr.ident "__liq_lock_ptr", id]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 32],
              user
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 64],
              YulExpr.hex 0x90e10dad8320b2f9ee6b84bebe89829c27a3fc1209e68031bc1d4b65c22e4da4
            ]),
            YulStmt.let_ "__liq_lock_slot"
              (YulExpr.call "keccak256" [YulExpr.ident "__liq_lock_ptr", YulExpr.lit 96]),
            YulStmt.let_ "__liq_lock_depth" (YulExpr.call "tload" [YulExpr.ident "__liq_lock_slot"]),
            YulStmt.if_ (YulExpr.call "gt" [YulExpr.ident "__liq_lock_depth", YulExpr.lit 0]) [
              YulStmt.assign "__liq_lock_depth"
                (YulExpr.call "sub" [YulExpr.ident "__liq_lock_depth", YulExpr.lit 1]),
              YulStmt.expr (YulExpr.call "tstore" [
                YulExpr.ident "__liq_lock_slot", YulExpr.ident "__liq_lock_depth"
              ])
            ],
            YulStmt.assign resultVar (YulExpr.ident "__liq_lock_depth")
          ]
        ]
    | _ => throw s!"midnightLiquidationLockDepthExitGet expects 2 args, got {args.length}"

def offerMarketIsHealthyModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightOfferMarketIsHealthy"
  numArgs := 2
  resultVars := [resultVar]
  writesState := false
  readsState := true
  axioms := ["midnight_offer_market_isHealthy_helper"]
  compile := fun _ctx args =>
    match args with
    | [id, user] =>
        pure [
          YulStmt.let_ resultVar (YulExpr.call "internal_internal_isHealthy" [
            YulExpr.call "add" [
              YulExpr.ident "offer_data_offset",
              YulExpr.call "calldataload" [YulExpr.ident "offer_data_offset"]
            ],
            id,
            user
          ])
        ]
    | _ => throw s!"midnightOfferMarketIsHealthy expects 2 args, got {args.length}"

def marketIdModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightMarketId"
  numArgs := 2
  resultVars := [resultVar]
  writesState := false
  readsState := true
  axioms := ["midnight_idlib_toid_market_abi_create2_preimage"]
  compile := fun _ctx args => do
    match args with
    | initialChainId :: selfAddress :: rest =>
        let marketDataOffset ←
          match rest with
          | [] => pure (YulExpr.ident "market_data_offset")
          | [offset] => pure offset
          | _ => throw s!"midnightMarketId expects 2 or 3 arguments, got {args.length}"
        let ptr := YulExpr.ident "__midnight_id_ptr"
        let tuplePtr := YulExpr.ident "__midnight_id_tuple_ptr"
        let collateralOffset := YulExpr.ident "__midnight_id_collateral_offset"
        let collateralLength := YulExpr.ident "__midnight_id_collateral_length"
        let collateralBytes := YulExpr.ident "__midnight_id_collateral_bytes"
        let abiLength := YulExpr.ident "__midnight_id_abi_length"
        let initcodeLength := YulExpr.ident "__midnight_id_initcode_length"
        let outerPtr := YulExpr.ident "__midnight_id_outer_ptr"
        pure [
          YulStmt.let_ resultVar (YulExpr.lit 0),
          YulStmt.block [
            YulStmt.let_ "__midnight_id_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [
              ptr,
              YulExpr.call "shl" [YulExpr.lit 168, YulExpr.hex 0x600b380380600b5f395ff3]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [ptr, YulExpr.lit 11],
              YulExpr.lit 32
            ]),
            YulStmt.let_ "__midnight_id_tuple_ptr"
              (YulExpr.call "add" [ptr, YulExpr.lit 43]),
            YulStmt.expr (YulExpr.call "mstore" [
              tuplePtr,
              YulExpr.call "calldataload" [marketDataOffset]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 32],
              YulExpr.lit 192
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 64],
              YulExpr.call "calldataload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 64]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 96],
              YulExpr.call "calldataload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 96]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 128],
              YulExpr.call "calldataload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 128]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 160],
              YulExpr.call "calldataload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 160]]
            ]),
            YulStmt.let_ "__midnight_id_collateral_offset" (YulExpr.call "add" [
              marketDataOffset,
              YulExpr.call "calldataload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 32]]
            ]),
            YulStmt.let_ "__midnight_id_collateral_length"
              (YulExpr.call "calldataload" [collateralOffset]),
            YulStmt.let_ "__midnight_id_collateral_bytes" (YulExpr.call "mul" [
              collateralLength,
              YulExpr.lit 128
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 192],
              collateralLength
            ]),
            YulStmt.expr (YulExpr.call "calldatacopy" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 224],
              YulExpr.call "add" [collateralOffset, YulExpr.lit 32],
              collateralBytes
            ]),
            YulStmt.let_ "__midnight_id_abi_length" (YulExpr.call "add" [
              YulExpr.lit 256,
              collateralBytes
            ]),
            YulStmt.let_ "__midnight_id_initcode_length" (YulExpr.call "add" [
              YulExpr.lit 11,
              abiLength
            ]),
            YulStmt.let_ "__midnight_id_inner_hash"
              (YulExpr.call "keccak256" [ptr, initcodeLength]),
            YulStmt.let_ "__midnight_id_outer_ptr" (YulExpr.call "add" [
              ptr,
              YulExpr.call "and" [
                YulExpr.call "add" [initcodeLength, YulExpr.lit 31],
                YulExpr.call "not" [YulExpr.lit 31]
              ]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              outerPtr,
              YulExpr.call "shl" [YulExpr.lit 248, YulExpr.lit 255]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [outerPtr, YulExpr.lit 1],
              YulExpr.call "shl" [YulExpr.lit 96, selfAddress]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [outerPtr, YulExpr.lit 21],
              initialChainId
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [outerPtr, YulExpr.lit 53],
              YulExpr.ident "__midnight_id_inner_hash"
            ]),
            YulStmt.assign resultVar
              (YulExpr.call "keccak256" [outerPtr, YulExpr.lit 85]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [outerPtr, YulExpr.lit 96]
            ])
          ]
        ]
    | _ =>
        throw s!"midnightMarketId expects 2 or 3 arguments, got {args.length}"

def marketIdAtOffsetModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightMarketIdAtOffset"
  numArgs := 3
  resultVars := [resultVar]
  writesState := false
  readsState := true
  axioms := ["midnight_idlib_toid_market_abi_create2_preimage"]
  compile := fun ctx args =>
    (marketIdModule resultVar).compile ctx args

def storeMarketInCodeModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightStoreMarketInCode"
  numArgs := 1
  resultVars := [resultVar]
  writesState := true
  readsState := false
  axioms := ["midnight_sstore2_market_initcode_layout", "create2_address_derivation"]
  compile := fun _ctx args => do
    match args with
    | [salt] =>
        let marketDataOffset := YulExpr.ident "market_data_offset"
        let ptr := YulExpr.ident "__midnight_store_ptr"
        let tuplePtr := YulExpr.ident "__midnight_store_tuple_ptr"
        let arrayPtr := YulExpr.ident "__midnight_store_array_ptr"
        let length := YulExpr.ident "__midnight_store_collateral_length"
        let collateralBytes := YulExpr.ident "__midnight_store_collateral_bytes"
        let dst := YulExpr.ident "__midnight_store_dst"
        let srcPtr := YulExpr.ident "__midnight_store_src_ptr"
        let srcEnd := YulExpr.ident "__midnight_store_src_end"
        let itemPtr := YulExpr.ident "__midnight_store_item_ptr"
        let abiLength := YulExpr.ident "__midnight_store_abi_length"
        let initcodeLength := YulExpr.ident "__midnight_store_initcode_length"
        pure [
          YulStmt.let_ resultVar (YulExpr.lit 0),
          YulStmt.block [
            YulStmt.let_ "__midnight_store_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [
              ptr,
              YulExpr.call "shl" [YulExpr.lit 168, YulExpr.hex 0x600b380380600b5f395ff3]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [ptr, YulExpr.lit 11],
              YulExpr.lit 32
            ]),
            YulStmt.let_ "__midnight_store_tuple_ptr"
              (YulExpr.call "add" [ptr, YulExpr.lit 43]),
            YulStmt.expr (YulExpr.call "mstore" [
              tuplePtr,
              YulExpr.call "and" [
                YulExpr.call "mload" [marketDataOffset],
                YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
              ]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 32],
              YulExpr.lit 192
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 64],
              YulExpr.call "mload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 64]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 96],
              YulExpr.call "mload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 96]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 128],
              YulExpr.call "and" [
                YulExpr.call "mload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 128]],
                YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
              ]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 160],
              YulExpr.call "and" [
                YulExpr.call "mload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 160]],
                YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
              ]
            ]),
            YulStmt.let_ "__midnight_store_array_ptr"
              (YulExpr.call "mload" [YulExpr.call "add" [marketDataOffset, YulExpr.lit 32]]),
            YulStmt.let_ "__midnight_store_collateral_length"
              (YulExpr.call "mload" [arrayPtr]),
            YulStmt.let_ "__midnight_store_collateral_bytes"
              (YulExpr.call "mul" [length, YulExpr.lit 128]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [tuplePtr, YulExpr.lit 192],
              length
            ]),
            YulStmt.let_ "__midnight_store_dst"
              (YulExpr.call "add" [tuplePtr, YulExpr.lit 224]),
            YulStmt.let_ "__midnight_store_src_ptr"
              (YulExpr.call "add" [arrayPtr, YulExpr.lit 32]),
            YulStmt.let_ "__midnight_store_src_end"
              (YulExpr.call "add" [srcPtr, YulExpr.call "mul" [length, YulExpr.lit 32]]),
            YulStmt.for_ [] (YulExpr.call "lt" [srcPtr, srcEnd])
              [YulStmt.assign "__midnight_store_src_ptr"
                (YulExpr.call "add" [srcPtr, YulExpr.lit 32])]
              [
                YulStmt.let_ "__midnight_store_item_ptr" (YulExpr.call "mload" [srcPtr]),
                YulStmt.expr (YulExpr.call "mstore" [
                  dst,
                  YulExpr.call "and" [
                    YulExpr.call "mload" [itemPtr],
                    YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
                  ]
                ]),
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [dst, YulExpr.lit 32],
                  YulExpr.call "mload" [YulExpr.call "add" [itemPtr, YulExpr.lit 32]]
                ]),
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [dst, YulExpr.lit 64],
                  YulExpr.call "mload" [YulExpr.call "add" [itemPtr, YulExpr.lit 64]]
                ]),
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [dst, YulExpr.lit 96],
                  YulExpr.call "and" [
                    YulExpr.call "mload" [YulExpr.call "add" [itemPtr, YulExpr.lit 96]],
                    YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
                  ]
                ]),
                YulStmt.assign "__midnight_store_dst"
                  (YulExpr.call "add" [dst, YulExpr.lit 128])
              ],
            YulStmt.let_ "__midnight_store_abi_length"
              (YulExpr.call "add" [YulExpr.lit 256, collateralBytes]),
            YulStmt.let_ "__midnight_store_initcode_length"
              (YulExpr.call "add" [YulExpr.lit 11, abiLength]),
            YulStmt.assign resultVar
              (YulExpr.call "create2" [YulExpr.lit 0, ptr, initcodeLength, salt]),
            YulStmt.if_ (YulExpr.call "iszero" [YulExpr.ident resultVar])
              [
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.lit 0,
                  YulExpr.call "shl" [YulExpr.lit 224, YulExpr.hex 0x4e487b71]
                ]),
                YulStmt.expr (YulExpr.call "mstore" [YulExpr.lit 4, YulExpr.lit 0x51]),
                YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 36])
              ],
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [
                ptr,
                YulExpr.call "and" [
                  YulExpr.call "add" [initcodeLength, YulExpr.lit 31],
                  YulExpr.call "not" [YulExpr.lit 31]
                ]
              ]
            ])
          ]
        ]
    | _ =>
        throw s!"midnightStoreMarketInCode expects 1 argument, got {args.length}"

def marketFromCodeModule (resultVar : String) : Compiler.ECM.ExternalCallModule where
  name := "midnightMarketFromCode"
  numArgs := 1
  resultVars := [resultVar]
  writesState := false
  readsState := true
  axioms := ["midnight_sstore2_market_runtime_abi_decode"]
  compile := fun _ctx args => do
    match args with
    | [id] =>
        let ptr := YulExpr.ident "__midnight_market_code_ptr"
        let dataPtr := YulExpr.ident "__midnight_market_data_ptr"
        let pointer := YulExpr.ident "__midnight_market_pointer"
        let length := YulExpr.ident "__midnight_market_code_length"
        let tuplePtr := YulExpr.ident "__midnight_market_tuple_ptr"
        let arrayOffset := YulExpr.ident "__midnight_market_array_offset"
        let arrayDataPtr := YulExpr.ident "__midnight_market_array_data_ptr"
        let arrayLength := YulExpr.ident "__midnight_market_array_length"
        let arrayPtr := YulExpr.ident "__midnight_market_array_ptr"
        let dst := YulExpr.ident "__midnight_market_dst"
        let src := YulExpr.ident "__midnight_market_src"
        let srcEnd := YulExpr.ident "__midnight_market_src_end"
        let itemPtr := YulExpr.ident "__midnight_market_item_ptr"
        pure [
          YulStmt.let_ resultVar (YulExpr.lit 0),
          YulStmt.block [
            YulStmt.let_ "__midnight_market_pointer"
              (YulExpr.call "and" [id, YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff]),
            YulStmt.let_ "__midnight_market_code_length"
              (YulExpr.call "extcodesize" [pointer]),
            YulStmt.let_ "__midnight_market_code_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [ptr, length]),
            YulStmt.let_ "__midnight_market_data_ptr"
              (YulExpr.call "add" [ptr, YulExpr.lit 32]),
            YulStmt.expr (YulExpr.call "extcodecopy" [
              pointer, dataPtr, YulExpr.lit 0, length
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [
                dataPtr,
                YulExpr.call "and" [
                  YulExpr.call "add" [length, YulExpr.lit 31],
                  YulExpr.call "not" [YulExpr.lit 31]
                ]
              ]
            ]),
            YulStmt.let_ "__midnight_market_tuple_ptr"
              (YulExpr.call "add" [ptr, YulExpr.call "mload" [dataPtr]]),
            YulStmt.let_ resultVar
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 192]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.ident resultVar,
              YulExpr.call "and" [
                YulExpr.call "mload" [YulExpr.call "add" [tuplePtr, YulExpr.lit 32]],
                YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
              ]
            ]),
            YulStmt.let_ "__midnight_market_array_offset"
              (YulExpr.call "mload" [YulExpr.call "add" [tuplePtr, YulExpr.lit 64]]),
            YulStmt.let_ "__midnight_market_array_data_ptr"
              (YulExpr.call "add" [
                YulExpr.call "add" [tuplePtr, arrayOffset],
                YulExpr.lit 32
              ]),
            YulStmt.let_ "__midnight_market_array_length"
              (YulExpr.call "mload" [arrayDataPtr]),
            YulStmt.let_ "__midnight_market_array_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "mstore" [arrayPtr, arrayLength]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
              YulExpr.call "add" [
                arrayPtr,
                YulExpr.call "and" [
                  YulExpr.call "add" [
                    YulExpr.call "add" [
                      YulExpr.lit 32,
                      YulExpr.call "mul" [arrayLength, YulExpr.lit 32]
                    ],
                    YulExpr.lit 31
                  ],
                  YulExpr.call "not" [YulExpr.lit 31]
                ]
              ]
            ]),
            YulStmt.let_ "__midnight_market_dst"
              (YulExpr.call "add" [arrayPtr, YulExpr.lit 32]),
            YulStmt.let_ "__midnight_market_src"
              (YulExpr.call "add" [arrayDataPtr, YulExpr.lit 32]),
            YulStmt.let_ "__midnight_market_src_end"
              (YulExpr.call "add" [src, YulExpr.call "mul" [arrayLength, YulExpr.lit 128]]),
            YulStmt.for_ [] (YulExpr.call "lt" [src, srcEnd])
              [YulStmt.assign "__midnight_market_src"
                (YulExpr.call "add" [src, YulExpr.lit 128])]
              [
                YulStmt.let_ "__midnight_market_item_ptr"
                  (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.lit Compiler.CompilationModel.freeMemoryPointer,
                  YulExpr.call "add" [itemPtr, YulExpr.lit 128]
                ]),
                YulStmt.expr (YulExpr.call "mstore" [
                  itemPtr,
                  YulExpr.call "and" [
                    YulExpr.call "mload" [src],
                    YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
                  ]
                ]),
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [itemPtr, YulExpr.lit 32],
                  YulExpr.call "mload" [YulExpr.call "add" [src, YulExpr.lit 32]]
                ]),
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [itemPtr, YulExpr.lit 64],
                  YulExpr.call "mload" [YulExpr.call "add" [src, YulExpr.lit 64]]
                ]),
                YulStmt.expr (YulExpr.call "mstore" [
                  YulExpr.call "add" [itemPtr, YulExpr.lit 96],
                  YulExpr.call "and" [
                    YulExpr.call "mload" [YulExpr.call "add" [src, YulExpr.lit 96]],
                    YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
                  ]
                ]),
                YulStmt.expr (YulExpr.call "mstore" [dst, itemPtr]),
                YulStmt.assign "__midnight_market_dst"
                  (YulExpr.call "add" [dst, YulExpr.lit 32])
              ],
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 32],
              arrayPtr
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 64],
              YulExpr.call "mload" [YulExpr.call "add" [tuplePtr, YulExpr.lit 96]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 96],
              YulExpr.call "mload" [YulExpr.call "add" [tuplePtr, YulExpr.lit 128]]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 128],
              YulExpr.call "and" [
                YulExpr.call "mload" [YulExpr.call "add" [tuplePtr, YulExpr.lit 160]],
                YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
              ]
            ]),
            YulStmt.expr (YulExpr.call "mstore" [
              YulExpr.call "add" [YulExpr.ident resultVar, YulExpr.lit 160],
              YulExpr.call "and" [
                YulExpr.call "mload" [YulExpr.call "add" [tuplePtr, YulExpr.lit 192]],
                YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff
              ]
            ])
          ]
        ]
    | _ =>
        throw s!"midnightMarketFromCode expects 1 argument, got {args.length}"

def marketReturnFromCodeModule : Compiler.ECM.ExternalCallModule where
  name := "midnightMarketReturnFromCode"
  numArgs := 1
  resultVars := []
  writesState := false
  readsState := true
  axioms := ["midnight_sstore2_market_runtime_abi_return"]
  compile := fun _ctx args => do
    match args with
    | [id] =>
        let pointer := YulExpr.ident "__midnight_market_return_pointer"
        let length := YulExpr.ident "__midnight_market_return_length"
        let ptr := YulExpr.ident "__midnight_market_return_ptr"
        pure [
          YulStmt.block [
            YulStmt.let_ "__midnight_market_return_pointer"
              (YulExpr.call "and" [id, YulExpr.hex 0xffffffffffffffffffffffffffffffffffffffff]),
            YulStmt.let_ "__midnight_market_return_length"
              (YulExpr.call "extcodesize" [pointer]),
            YulStmt.let_ "__midnight_market_return_ptr"
              (YulExpr.call "mload" [YulExpr.lit Compiler.CompilationModel.freeMemoryPointer]),
            YulStmt.expr (YulExpr.call "extcodecopy" [
              pointer, ptr, YulExpr.lit 0, length
            ]),
            YulStmt.expr (YulExpr.call "return" [ptr, length])
          ]
        ]
    | _ =>
        throw s!"midnightMarketReturnFromCode expects 1 argument, got {args.length}"

def arrayLength {α : Type} (values : Array α) : Uint256 := Contracts.arrayLength values
def arrayElement {α : Type} [Inhabited α] (values : Array α) (index : Uint256) : α :=
  Contracts.arrayElement values index
def safeTransfer (token toAddr : Address) (amount : Uint256) : Contract Unit :=
  Contracts.safeTransfer token toAddr amount
def safeTransferFrom (token fromAddr toAddr : Address) (amount : Uint256) : Contract Unit :=
  Contracts.safeTransferFrom token fromAddr toAddr amount
def bitAnd (a b : Uint256) : Uint256 := Verity.Core.Uint256.and a b
def bitOr (a b : Uint256) : Uint256 := Verity.Core.Uint256.or a b
def bitNot (a : Uint256) : Uint256 := Verity.Core.Uint256.not a
def shl (shift value : Uint256) : Uint256 := Verity.Core.Uint256.shl shift value
def div (a b : Uint256) : Uint256 := Verity.Core.Uint256.div a b
def mod (a b : Uint256) : Uint256 := Verity.Core.Uint256.mod a b
def calldataload (offset : Uint256) : Uint256 := offset

def solidityPanicModule : Compiler.ECM.ExternalCallModule where
  name := "solidityPanic"
  numArgs := 1
  resultVars := []
  writesState := false
  readsState := false
  axioms := ["solidity_panic_revert_payload"]
  compile := fun _ctx args =>
    match args with
    | [code] =>
        pure [
          YulStmt.expr (YulExpr.call "mstore" [
            YulExpr.lit 0,
            YulExpr.call "shl" [YulExpr.lit 224, YulExpr.lit 0x4e487b71]
          ]),
          YulStmt.expr (YulExpr.call "mstore" [YulExpr.lit 4, code]),
          YulStmt.expr (YulExpr.call "revert" [YulExpr.lit 0, YulExpr.lit 36])
        ]
    | _ => throw s!"solidityPanic expects 1 arg, got {args.length}"

/-
  A focused Verity-facing Midnight model for the RCF arithmetic used by
  `src/Midnight.sol::liquidate`.

  The surface targets the normal-mode arithmetic that the Lean proofs in
  `Midnight.Proofs.RCF` reason about:

    * `maxRepaid` lines 655-661,
    * seized-input `repaidUnits` lines 649-650,
    * repaid-input `seizedAssets` lines 649-653,
    * `zeroFloorSub` shape used by the RCF guard,
    * `isHealthy` collateral contribution lines 953-955,
    * a focused normal-mode max-repaid post-health projection,
    * bad-debt collateral-loop repayable value lines 610-616,
    * one-step collateral-loop accumulator update lines 609-616,
    * bad-debt `lossFactor`, `totalUnits`, and continuous-fee-credit updates
      lines 626-640,
    * the `atMostOneNonZero` input guard line 592,
    * the borrower and liquidator-gate guards lines 593-597,
    * the full and normal-mode `NotLiquidatable` guard lines 620-623,
    * the bad-debt branch predicate line 625,
    * the bad-debt borrower debt write line 628,
    * a combined bad-debt branch local sequencing surface for lines 625-640,
    * post-maturity and normal-mode LIF selection lines 644-647,
    * the repay/seize branch predicate line 645,
    * normal-mode RCF guard lines 662-667,
    * a general normal-mode RCF local sequence for both repay-input and
      seize-input amount branches,
    * collateral bitmap clearing lines 679-681,
    * withdrawable increase line 682,
    * a combined normal-mode repaid-input local sequencing surface for lines
      625-717, specialized to `repaidUnits = maxRepaid` after the RCF guard's
      left disjunct is already satisfied,
    * a single normal-mode local `liquidate` sequence that combines the
      bad-debt market writes, max-repay branch, borrower/collateral writes,
      payer/callback branch, and return pair from lines 625-717 under the same
      `repaidUnits = maxRepaid` specialization,
    * payer/callback branch and returned pair lines 686, 704-717,
    * `updatePositionView` post-slash credit, pending-fee slashing, and
      accrued-fee subtraction lines 802-818, including the returned triple.
-/
verity_contract MidnightRCF where
  storage
    unusedSlot : Uint256 := slot 0

  function maxRepaid (debt : Uint256, maxDebt : Uint256, lif : Uint256, lltv : Uint256) :
      Uint256 := do
    if lltv < 1000000000000000000 then
      return mulDivUp
        (sub debt maxDebt)
        (mul 1000000000000000000 1000000000000000000)
        (sub
          (mul 1000000000000000000 1000000000000000000)
          (mul lif lltv))
    else
      return 115792089237316195423570985008687907853269984665640564039457584007913129639935

  function seizedFromRepaid
      (repaidUnits : Uint256, lif : Uint256, liquidatedCollatPrice : Uint256) :
      Uint256 := do
    let repayValue := mulDivDown repaidUnits lif 1000000000000000000
    return mulDivDown repayValue
      1000000000000000000000000000000000000
      liquidatedCollatPrice

  function repaidFromSeized
      (seizedAssets : Uint256, lif : Uint256, liquidatedCollatPrice : Uint256) :
      Uint256 := do
    let seizedValue := mulDivUp seizedAssets liquidatedCollatPrice
      1000000000000000000000000000000000000
    return mulDivUp seizedValue 1000000000000000000 lif

  function repaySeizeAmounts
      (inputRepaidUnits : Uint256, inputSeizedAssets : Uint256, lif : Uint256,
        liquidatedCollatPrice : Uint256) : Tuple [Uint256, Uint256] := do
    if inputSeizedAssets > 0 then
      let repaidUnits := mulDivUp
        (mulDivUp inputSeizedAssets liquidatedCollatPrice
          1000000000000000000000000000000000000)
        1000000000000000000
        lif
      return (repaidUnits, inputSeizedAssets)
    else
      let seizedAssets := mulDivDown
        (mulDivDown inputRepaidUnits lif 1000000000000000000)
        1000000000000000000000000000000000000
        liquidatedCollatPrice
      return (inputRepaidUnits, seizedAssets)

  function collateralMaxDebt (collateral : Uint256, price : Uint256, lltv : Uint256) :
      Uint256 := do
    let quoted := mulDivDown collateral price
      1000000000000000000000000000000000000
    return mulDivDown quoted lltv 1000000000000000000

  function postDebt (debt : Uint256, repaidUnits : Uint256) : Uint256 := do
    return sub debt repaidUnits

  function postCollateral (collateral : Uint256, seizedAssets : Uint256) : Uint256 := do
    return sub collateral seizedAssets

  function zeroFloorSub (x : Uint256, y : Uint256) : Uint256 := do
    if x > y then
      return sub x y
    else
      return 0

  function withdrawableAfterRepay (withdrawable : Uint256, repaidUnits : Uint256) :
      Uint256 := do
    return add withdrawable repaidUnits

  function clearBitmapBit (bitmap : Uint256, selectedIndex : Uint256) : Uint256 := do
    let mask := bitNot (shl selectedIndex 1)
    return bitAnd bitmap mask

  function atMostOneNonZero (repaidUnits : Uint256, seizedAssets : Uint256) :
      Bool := do
    return (repaidUnits == 0 || seizedAssets == 0)

  function borrowerDebtPositive (debt : Uint256) : Bool := do
    return debt > 0

  function liquidatorGateAllows (liquidatorGate : Address, canLiquidate : Bool) :
      Bool := do
    return (liquidatorGate == 0 || canLiquidate)

  function normalModeLiquidatable
      (liquidationUnlocked : Bool, originalDebt : Uint256, maxDebt : Uint256) :
      Bool := do
    return (liquidationUnlocked && originalDebt > maxDebt)

  function liquidatableGuard
      (liquidationLocked : Bool, postMaturityMode : Bool, blockTimestamp : Uint256,
        maturity : Uint256, originalDebt : Uint256, maxDebt : Uint256) :
      Bool := do
    if postMaturityMode then
      return (liquidationLocked == false && blockTimestamp > maturity)
    else
      return (liquidationLocked == false && originalDebt > maxDebt)

  function normalModeEntryGuards
      (repaidUnits : Uint256, seizedAssets : Uint256, debt : Uint256,
        liquidatorGate : Address, canLiquidate : Bool, liquidationUnlocked : Bool,
        originalDebt : Uint256, maxDebt : Uint256) : Bool := do
    return ((repaidUnits == 0 || seizedAssets == 0) && debt > 0 &&
      (liquidatorGate == 0 || canLiquidate) &&
      (liquidationUnlocked && originalDebt > maxDebt))

  function repayBranchActive (repaidUnits : Uint256, seizedAssets : Uint256) :
      Bool := do
    return (repaidUnits > 0 || seizedAssets > 0)

  function liquidationLif
      (postMaturityMode : Bool, maxLif : Uint256, blockTimestamp : Uint256,
        maturity : Uint256) : Uint256 := do
    if postMaturityMode then
      let elapsed := sub blockTimestamp maturity
      let postMaturityLif := add 1000000000000000000
        (mulDivDown
          (sub maxLif 1000000000000000000)
          elapsed
          900)
      if maxLif <= postMaturityLif then
        return maxLif
      else
        return postMaturityLif
    else
      return maxLif

  function badDebtBranchActive (badDebt : Uint256) : Bool := do
    return badDebt > 0

  function badDebtPostDebt (originalDebt : Uint256, badDebt : Uint256) : Uint256 := do
    return sub originalDebt badDebt

  function badDebtLocalSequence
      (originalDebt : Uint256, totalUnits : Uint256, oldLossFactor : Uint256,
        continuousFeeCredit : Uint256, badDebt : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Bool] := do
    let mut postDebt := originalDebt
    let mut newTotalUnits := totalUnits
    let mut newLossFactor := oldLossFactor
    let mut newContinuousFeeCredit := continuousFeeCredit
    let branchActive := badDebt > 0
    if branchActive then
      postDebt := sub originalDebt badDebt
      newLossFactor := sub
        340282366920938463463374607431768211455
        (mulDivDown
          (sub 340282366920938463463374607431768211455 oldLossFactor)
          (sub totalUnits badDebt)
          totalUnits)
      newTotalUnits := sub totalUnits badDebt
      if oldLossFactor < 340282366920938463463374607431768211455 then
        newContinuousFeeCredit := mulDivDown continuousFeeCredit
          (sub 340282366920938463463374607431768211455 newLossFactor)
          (sub 340282366920938463463374607431768211455 oldLossFactor)
      else
        newContinuousFeeCredit := 0
    else
      postDebt := postDebt
    return (postDebt, newTotalUnits, newLossFactor, newContinuousFeeCredit, branchActive)

  function callbackRequired (callback : Address) : Bool := do
    return callback != 0

  function solidityPanic (code : Uint256) : Unit := do
    ecmDo solidityPanicModule [code]

  function liquidatePayer (callback : Address, msgSender : Address) : Address := do
    if callback != 0 then
      return callback
    else
      return msgSender

  function callbackSuccessValue () : Uint256 := do
    return 57683088179238363159977504707935902064464440500167392520350201799917296135842

  function callbackReturnAccepted (callback : Address, returnValue : Uint256) :
      Bool := do
    return (callback == 0 ||
      returnValue ==
        57683088179238363159977504707935902064464440500167392520350201799917296135842)

  function liquidateReturn (seizedAssets : Uint256, repaidUnits : Uint256) :
      Tuple [Uint256, Uint256] := do
    return (seizedAssets, repaidUnits)

  function normalModeRcfLocalSequence
      (currentDebt : Uint256, maxDebt : Uint256, inputRepaidUnits : Uint256,
        inputSeizedAssets : Uint256, lltv : Uint256, lif : Uint256,
        liquidatedCollatPrice : Uint256, collateral : Uint256,
        collateralBitmap : Uint256, collateralIndex : Uint256, withdrawable : Uint256,
        rcfThreshold : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256,
        Bool, Uint256] := do
    let mut repaidUnits := inputRepaidUnits
    let mut seizedAssets := inputSeizedAssets
    if inputSeizedAssets > 0 then
      repaidUnits := mulDivUp
        (mulDivUp inputSeizedAssets liquidatedCollatPrice
          1000000000000000000000000000000000000)
        1000000000000000000
        lif
    else
      seizedAssets := mulDivDown
        (mulDivDown inputRepaidUnits lif 1000000000000000000)
        1000000000000000000000000000000000000
        liquidatedCollatPrice
    let mut maxRepaid :=
      115792089237316195423570985008687907853269984665640564039457584007913129639935
    if lltv < 1000000000000000000 then
      maxRepaid := mulDivUp
        (sub currentDebt maxDebt)
        (mul 1000000000000000000 1000000000000000000)
        (sub
          (mul 1000000000000000000 1000000000000000000)
          (mul lif lltv))
    else
      maxRepaid := maxRepaid
    let collateralRepayCapacity := mulDivDown
      (mulDivDown collateral liquidatedCollatPrice
        1000000000000000000000000000000000000)
      1000000000000000000
      lif
    let mut capacityShortfall := 0
    if collateralRepayCapacity > maxRepaid then
      capacityShortfall := sub collateralRepayCapacity maxRepaid
    else
      capacityShortfall := capacityShortfall
    let rcfAccepted := repaidUnits <= maxRepaid ||
      capacityShortfall < rcfThreshold
    let newCollateral := sub collateral seizedAssets
    let mut postBitmap := collateralBitmap
    if newCollateral == 0 && seizedAssets > 0 then
      postBitmap := bitAnd collateralBitmap (bitNot (shl collateralIndex 1))
    else
      postBitmap := postBitmap
    let postWithdrawable := add withdrawable repaidUnits
    let finalDebt := sub currentDebt repaidUnits
    return (repaidUnits, seizedAssets, maxRepaid, newCollateral, postBitmap,
      postWithdrawable, finalDebt, rcfAccepted, capacityShortfall)

  function normalModeRepaidInputLocalSequence
      (originalDebt : Uint256, badDebt : Uint256, maxDebt : Uint256,
        lltv : Uint256, lif : Uint256, liquidatedCollatPrice : Uint256,
        collateral : Uint256, collateralBitmap : Uint256, collateralIndex : Uint256,
        withdrawable : Uint256, callback : Address, msgSender : Address,
        callbackReturn : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256,
        Address, Bool, Uint256, Uint256] := do
    let currentDebt := sub originalDebt badDebt
    let mut maxRepaid :=
      115792089237316195423570985008687907853269984665640564039457584007913129639935
    if lltv < 1000000000000000000 then
      maxRepaid := mulDivUp
        (sub currentDebt maxDebt)
        (mul 1000000000000000000 1000000000000000000)
        (sub
          (mul 1000000000000000000 1000000000000000000)
          (mul lif lltv))
    else
      maxRepaid := maxRepaid
    let repaidUnits := maxRepaid
    let repayValue := mulDivDown repaidUnits lif 1000000000000000000
    let seizedAssets := mulDivDown repayValue
      1000000000000000000000000000000000000
      liquidatedCollatPrice
    let newCollateral := sub collateral seizedAssets
    let mut postBitmap := collateralBitmap
    if newCollateral == 0 && seizedAssets > 0 then
      postBitmap := bitAnd collateralBitmap (bitNot (shl collateralIndex 1))
    else
      postBitmap := postBitmap
    let postWithdrawable := add withdrawable repaidUnits
    let finalDebt := sub currentDebt repaidUnits
    let mut payer := msgSender
    if callback != 0 then
      payer := callback
    else
      payer := payer
    let callbackAccepted := callback == 0 ||
      callbackReturn ==
        57683088179238363159977504707935902064464440500167392520350201799917296135842
    return (currentDebt, maxRepaid, seizedAssets, newCollateral, postBitmap,
      postWithdrawable, finalDebt, payer, callbackAccepted,
      seizedAssets, repaidUnits)

  function normalModeLiquidateLocalSequence
      (originalDebt : Uint256, totalUnits : Uint256, oldLossFactor : Uint256,
        continuousFeeCredit : Uint256, badDebt : Uint256, maxDebt : Uint256,
        lltv : Uint256, lif : Uint256, liquidatedCollatPrice : Uint256,
        collateral : Uint256, collateralBitmap : Uint256, collateralIndex : Uint256,
        withdrawable : Uint256, callback : Address, msgSender : Address,
        callbackReturn : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256,
        Uint256, Uint256, Uint256, Uint256, Address, Bool, Uint256, Uint256] := do
    let mut postBadDebtDebt := originalDebt
    let mut newTotalUnits := totalUnits
    let mut newLossFactor := oldLossFactor
    let mut newContinuousFeeCredit := continuousFeeCredit
    if badDebt > 0 then
      postBadDebtDebt := sub originalDebt badDebt
      newLossFactor := sub
        340282366920938463463374607431768211455
        (mulDivDown
          (sub 340282366920938463463374607431768211455 oldLossFactor)
          (sub totalUnits badDebt)
          totalUnits)
      newTotalUnits := sub totalUnits badDebt
      if oldLossFactor < 340282366920938463463374607431768211455 then
        newContinuousFeeCredit := mulDivDown continuousFeeCredit
          (sub 340282366920938463463374607431768211455 newLossFactor)
          (sub 340282366920938463463374607431768211455 oldLossFactor)
      else
        newContinuousFeeCredit := 0
    else
      postBadDebtDebt := postBadDebtDebt
    let currentDebt := postBadDebtDebt
    let mut maxRepaid :=
      115792089237316195423570985008687907853269984665640564039457584007913129639935
    if lltv < 1000000000000000000 then
      maxRepaid := mulDivUp
        (sub currentDebt maxDebt)
        (mul 1000000000000000000 1000000000000000000)
        (sub
          (mul 1000000000000000000 1000000000000000000)
          (mul lif lltv))
    else
      maxRepaid := maxRepaid
    let repaidUnits := maxRepaid
    let repayValue := mulDivDown repaidUnits lif 1000000000000000000
    let seizedAssets := mulDivDown repayValue
      1000000000000000000000000000000000000
      liquidatedCollatPrice
    let newCollateral := sub collateral seizedAssets
    let mut postBitmap := collateralBitmap
    if newCollateral == 0 && seizedAssets > 0 then
      postBitmap := bitAnd collateralBitmap (bitNot (shl collateralIndex 1))
    else
      postBitmap := postBitmap
    let postWithdrawable := add withdrawable repaidUnits
    let finalDebt := sub currentDebt repaidUnits
    let mut payer := msgSender
    if callback != 0 then
      payer := callback
    else
      payer := payer
    let callbackAccepted := callback == 0 ||
      callbackReturn ==
        57683088179238363159977504707935902064464440500167392520350201799917296135842
    return (postBadDebtDebt, newTotalUnits, newLossFactor, newContinuousFeeCredit,
      currentDebt, maxRepaid, seizedAssets, newCollateral, postBitmap,
      postWithdrawable, finalDebt, payer, callbackAccepted,
      seizedAssets, repaidUnits)

  function badDebtCollateralRepayable
      (collateral : Uint256, price : Uint256, maxLif : Uint256) :
      Uint256 := do
    let quoted := mulDivUp collateral price
      1000000000000000000000000000000000000
    return mulDivUp quoted 1000000000000000000 maxLif

  function collateralLoopStep
      (maxDebt : Uint256, liquidatedCollatPrice : Uint256, badDebt : Uint256,
        collateralBitmap : Uint256, slotIndex : Uint256, selectedIndex : Uint256,
        collateral : Uint256, price : Uint256, lltv : Uint256, maxLif : Uint256) :
      Tuple [Uint256, Uint256, Uint256, Uint256] := do
    let collateralQuote := mulDivDown collateral price
      1000000000000000000000000000000000000
    let nextMaxDebt := add maxDebt
      (mulDivDown collateralQuote lltv 1000000000000000000)
    let mut nextLiquidatedCollatPrice := liquidatedCollatPrice
    if slotIndex == selectedIndex then
      nextLiquidatedCollatPrice := price
    else
      nextLiquidatedCollatPrice := nextLiquidatedCollatPrice
    let repayableQuote := mulDivUp collateral price
      1000000000000000000000000000000000000
    let repayable := mulDivUp repayableQuote 1000000000000000000 maxLif
    let mut nextBadDebt := 0
    if badDebt > repayable then
      nextBadDebt := sub badDebt repayable
    else
      nextBadDebt := nextBadDebt
    let nextBitmap := bitAnd collateralBitmap (bitNot (shl slotIndex 1))
    return (nextMaxDebt, nextLiquidatedCollatPrice, nextBadDebt, nextBitmap)

  function rcfAllows
      (repaidUnits : Uint256, maxRepaid : Uint256, collateral : Uint256,
        liquidatedCollatPrice : Uint256, lif : Uint256, rcfThreshold : Uint256) :
      Bool := do
    let collateralRepayCapacity := mulDivDown
      (mulDivDown collateral liquidatedCollatPrice
        1000000000000000000000000000000000000)
      1000000000000000000
      lif
    return (repaidUnits <= maxRepaid ||
      sub collateralRepayCapacity maxRepaid < rcfThreshold)

  function rcfAllowsZeroFloor
      (repaidUnits : Uint256, maxRepaid : Uint256, collateral : Uint256,
        liquidatedCollatPrice : Uint256, lif : Uint256, rcfThreshold : Uint256) :
      Bool := do
    let collateralRepayCapacity := mulDivDown
      (mulDivDown collateral liquidatedCollatPrice
        1000000000000000000000000000000000000)
      1000000000000000000
      lif
    let mut capacityShortfall := 0
    if collateralRepayCapacity > maxRepaid then
      capacityShortfall := sub collateralRepayCapacity maxRepaid
    else
      capacityShortfall := capacityShortfall
    return (repaidUnits <= maxRepaid || capacityShortfall < rcfThreshold)

  function normalModeMaxRepaidHealthyWithin3
      (debt : Uint256, otherMaxDebt : Uint256, collateral : Uint256, price : Uint256,
        lif : Uint256, lltv : Uint256) :
      Bool := do
    let selectedQuote := mulDivDown collateral price
      1000000000000000000000000000000000000
    let selectedMaxDebt := mulDivDown selectedQuote lltv 1000000000000000000
    let maxDebt := add otherMaxDebt selectedMaxDebt
    let repaid := mulDivUp
      (sub debt maxDebt)
      (mul 1000000000000000000 1000000000000000000)
      (sub
        (mul 1000000000000000000 1000000000000000000)
        (mul lif lltv))
    let repayValue := mulDivDown repaid lif 1000000000000000000
    let seizedAssets := mulDivDown repayValue
      1000000000000000000000000000000000000
      price
    let postDebt := sub debt repaid
    let postCollateral := sub collateral seizedAssets
    let postQuote := mulDivDown postCollateral price
      1000000000000000000000000000000000000
    let postSelectedMaxDebt := mulDivDown postQuote lltv 1000000000000000000
    let postMaxDebt := add otherMaxDebt postSelectedMaxDebt
    return postDebt <= add postMaxDebt 3

  function lossFactorAfterBadDebt
      (totalUnits : Uint256, badDebt : Uint256, oldLossFactor : Uint256) :
      Uint256 := do
    return sub
      340282366920938463463374607431768211455
      (mulDivDown
        (sub 340282366920938463463374607431768211455 oldLossFactor)
        (sub totalUnits badDebt)
        totalUnits)

  function totalUnitsAfterBadDebt (totalUnits : Uint256, badDebt : Uint256) : Uint256 := do
    return sub totalUnits badDebt

  function continuousFeeCreditAfterBadDebt
      (continuousFeeCredit : Uint256, oldLossFactor : Uint256, newLossFactor : Uint256) :
      Uint256 := do
    if oldLossFactor < 340282366920938463463374607431768211455 then
      return mulDivDown continuousFeeCredit
        (sub 340282366920938463463374607431768211455 newLossFactor)
        (sub 340282366920938463463374607431768211455 oldLossFactor)
    else
      return 0

  function badDebtCoversTwoPostSlashCredits
      (totalUnits : Uint256, badDebt : Uint256, oldLossFactor : Uint256,
        credit0 : Uint256, credit1 : Uint256) :
      Bool := do
    let newLossFactor := sub
      340282366920938463463374607431768211455
      (mulDivDown
        (sub 340282366920938463463374607431768211455 oldLossFactor)
        (sub totalUnits badDebt)
        totalUnits)
    let newTotalUnits := sub totalUnits badDebt
    let postSlashCredit0 := mulDivDown credit0
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    let postSlashCredit1 := mulDivDown credit1
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    return add postSlashCredit0 postSlashCredit1 <= newTotalUnits

  function badDebtCoversTwoPostUpdateCredits
      (totalUnits : Uint256, badDebt : Uint256, oldLossFactor : Uint256,
        credit0 : Uint256, accruedFee0 : Uint256, credit1 : Uint256, accruedFee1 : Uint256) :
      Bool := do
    let newLossFactor := sub
      340282366920938463463374607431768211455
      (mulDivDown
        (sub 340282366920938463463374607431768211455 oldLossFactor)
        (sub totalUnits badDebt)
        totalUnits)
    let newTotalUnits := sub totalUnits badDebt
    let postSlashCredit0 := mulDivDown credit0
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    let postSlashCredit1 := mulDivDown credit1
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    let postUpdateCredit0 := sub postSlashCredit0 accruedFee0
    let postUpdateCredit1 := sub postSlashCredit1 accruedFee1
    return add postUpdateCredit0 postUpdateCredit1 <= newTotalUnits

  -- Same slashing formula as `badDebtCoversTwoPostSlashCredits`, kept under a
  -- storage-oriented name because the proof consumes it after lender updates.
  function badDebtCoversTwoStoredCreditsAfterUpdates
      (totalUnits : Uint256, badDebt : Uint256, oldLossFactor : Uint256,
        credit0 : Uint256, credit1 : Uint256) :
      Bool := do
    let newLossFactor := sub
      340282366920938463463374607431768211455
      (mulDivDown
        (sub 340282366920938463463374607431768211455 oldLossFactor)
        (sub totalUnits badDebt)
        totalUnits)
    let newTotalUnits := sub totalUnits badDebt
    let storedCredit0 := mulDivDown credit0
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    let storedCredit1 := mulDivDown credit1
      (sub 340282366920938463463374607431768211455 newLossFactor)
      (sub 340282366920938463463374607431768211455 oldLossFactor)
    return add storedCredit0 storedCredit1 <= newTotalUnits

  function postSlashCredit
      (credit : Uint256, marketLossFactor : Uint256, lastLossFactor : Uint256) :
      Uint256 := do
    if lastLossFactor < 340282366920938463463374607431768211455 then
      return mulDivDown credit
        (sub 340282366920938463463374607431768211455 marketLossFactor)
            (sub 340282366920938463463374607431768211455 lastLossFactor)
    else
      return 0

  function postSlashPendingFee
      (pendingFee : Uint256, credit : Uint256, postSlashCredit : Uint256) :
      Uint256 := do
    if 0 < credit then
      return sub pendingFee
        (mulDivUp pendingFee (sub credit postSlashCredit) credit)
    else
      return 0

  function accruedFee
      (postSlashPendingFee : Uint256, lastAccrual : Uint256, accrualEnd : Uint256,
        maturity : Uint256) :
      Uint256 := do
    if lastAccrual < maturity then
      return mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub maturity lastAccrual)
    else
      return 0

  function postUpdateCreditAfterFee (postSlashCredit : Uint256, accruedFee : Uint256) :
      Uint256 := do
    return sub postSlashCredit accruedFee

  function updatePositionViewSequence
      (credit : Uint256, lastLossFactor : Uint256, marketLossFactor : Uint256,
        pendingFee : Uint256, lastAccrual : Uint256, blockTimestamp : Uint256,
        maturity : Uint256) : Tuple [Uint256, Uint256, Uint256] := do
    let mut postSlashCredit := 0
    if lastLossFactor < 340282366920938463463374607431768211455 then
      postSlashCredit := mulDivDown credit
        (sub 340282366920938463463374607431768211455 marketLossFactor)
        (sub 340282366920938463463374607431768211455 lastLossFactor)
    else
      postSlashCredit := postSlashCredit
    let mut postSlashPendingFee := 0
    if credit > 0 then
      postSlashPendingFee := sub pendingFee
        (mulDivUp pendingFee (sub credit postSlashCredit) credit)
    else
      postSlashPendingFee := postSlashPendingFee
    let mut accrualEnd := maturity
    if blockTimestamp <= maturity then
      accrualEnd := blockTimestamp
    else
      accrualEnd := accrualEnd
    let mut fee := 0
    if lastAccrual < maturity then
      fee := mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub maturity lastAccrual)
    else
      fee := fee
    return (sub postSlashCredit fee, sub postSlashPendingFee fee, fee)

/-
  Initial full-artifact scaffold for `morpho-midnight/src/Midnight.sol`.

  This is intentionally separate from `MidnightRCF`. It starts the executable
  `Midnight` surface required by upstream Foundry parity with constructor-visible
  role slots, simple mappings/getters, `setIsAuthorized`, `setConsumed`, and the
  low-risk fee/role setter paths. Behavioral entrypoints such as `take`,
  `withdraw`, `repay`, `touchMarket`, and `liquidate` still need the remaining
  full-contract implementation work documented in `docs/MIDNIGHT_VERITY_PLAN.md`.
-/
verity_contract Midnight where
  storage
    /- Solidity keeps INITIAL_CHAIN_ID as an immutable rather than storage. Verity
       does not yet have immutable bytecode fields, so the model isolates the
       constructor-captured value outside Solidity's declared storage range. -/
    initialChainIdSlot : Uint256 := slot 1024
    positionSlot : MappingStruct2(Bytes32,Address,[
      credit @word 0 packed(0,128),
      pendingFee @word 0 packed(128,128),
      lastLossFactor @word 1 packed(0,128),
      lastAccrual @word 1 packed(128,128),
      debt @word 2 packed(0,128),
      collateralBitmap @word 2 packed(128,128),
      collateral : FixedArray Uint256 128 @word 3
    ]) := slot 0
    marketStateSlot : MappingStruct(Bytes32,[
      totalUnits @word 0 packed(0,128),
      lossFactor @word 0 packed(128,128),
      withdrawable @word 1 packed(0,128),
      continuousFeeCredit @word 1 packed(128,128),
      settlementFeeCbp0 @word 2 packed(0,16),
      settlementFeeCbp1 @word 2 packed(16,16),
      settlementFeeCbp2 @word 2 packed(32,16),
      settlementFeeCbp3 @word 2 packed(48,16),
      settlementFeeCbp4 @word 2 packed(64,16),
      settlementFeeCbp5 @word 2 packed(80,16),
      settlementFeeCbp6 @word 2 packed(96,16),
      continuousFee @word 2 packed(112,32),
      tickSpacing @word 2 packed(144,8)
    ]) := slot 1
    consumedSlot : MappingStruct2(Address,Bytes32,[amount @word 0]) := slot 2
    isAuthorizedSlot : MappingStruct2(Address,Address,[flag @word 0]) := slot 3
    defaultSettlementFeeSlot : MappingStruct2(Address,Uint256,[fee @word 0]) := slot 4
    defaultContinuousFeeSlot : Address -> Uint256 := slot 5
    claimableSettlementFeeSlot : Address -> Uint256 := slot 6
    roleSetterSlot : Address := slot 7
    feeSetterSlot : Address := slot 8
    feeClaimerSlot : Address := slot 9
    tickSpacingSetterSlot : Address := slot 10

  struct CollateralParams where
    token : Address,
    lltv : Uint256,
    maxLif : Uint256,
    oracle : Address

  struct Market where
    loanToken : Address,
    collateralParams : Array CollateralParams,
    maturity : Uint256,
    rcfThreshold : Uint256,
    enterGate : Address,
    liquidatorGate : Address

  struct Offer where
    market : Market,
    buy : Bool,
    maker : Address,
    start : Uint256,
    expiry : Uint256,
    tick : Uint256,
    group : Bytes32,
    callback : Address,
    callbackData : Bytes,
    receiverIfMakerIsSeller : Address,
    ratifier : Address,
    reduceOnly : Bool,
    maxUnits : Uint256,
    maxAssets : Uint256

  errors
    error AlreadyConsumed()
    error CannotIncreaseDebtPostMaturity()
    error CastOverflow()
    error CollateralParamsNotSorted()
    error ContinuousFeeTooHigh()
    error ConsumedAssets()
    error ConsumedUnits()
    error BuyerGatedFromIncreasingCredit()
    error FeeNotMultipleOfFeeCbp()
    error InconsistentInput()
    error InvalidFeeIndex()
    error InvalidMaxLif()
    error InvalidTickSpacing()
    error LiquidatorGatedFromLiquidating()
    error LltvNotAllowed()
    error MarketNotCreated()
    error MarketLossFactorMaxedOut()
    error MaturityTooFar()
    error MultipleNonZero()
    error NoCollateralParams()
    error NotBorrower()
    error NotLiquidatable()
    error OfferExpired()
    error OfferNotStarted()
    error OnlyFeeClaimer()
    error OnlyFeeSetter()
    error OnlyRoleSetter()
    error OnlyTickSpacingSetter()
    error MakerCreditOrDebtIncreased()
    error RatifierUnauthorized()
    error RatifierFail()
    error RecoveryCloseFactorConditionsViolated()
    error SelfTake()
    error SellerGatedFromIncreasingDebt()
    error SellerIsLiquidatable()
    error SettlementFeeTooHigh()
    error TakerUnauthorized()
    error TickNotAccessible()
    error TickOutOfRange()
    error TooManyActivatedCollaterals()
    error TooManyCollateralParams()
    error Unauthorized()
    error UnhealthyBorrower()
    error WrongBuyCallbackReturnValue()
    error WrongFlashLoanCallbackReturnValue()
    error WrongLiquidateCallbackReturnValue()
    error WrongSellCallbackReturnValue()

  event_defs
    event UpdatePosition(@indexed id_ : Bytes32, @indexed user : Address, creditDecrease : Uint256, pendingFeeDecrease : Uint256, accruedFee : Uint256)
    event Take(caller : Address, @indexed id_ : Bytes32, units : Uint256, @indexed taker : Address, @indexed maker : Address, offerIsBuy : Bool, group : Bytes32, buyerAssets : Uint256, sellerAssets : Uint256, consumed : Uint256, buyerPendingFeeIncrease : Uint256, sellerPendingFeeDecrease : Uint256, buyerCreditIncrease : Uint256, sellerCreditDecrease : Uint256, receiver : Address, payer : Address)
    event Withdraw(caller : Address, @indexed id_ : Bytes32, units : Uint256, @indexed onBehalf : Address, @indexed receiver : Address, pendingFeeDecrease : Uint256)
    event Liquidate(caller : Address, @indexed id_ : Bytes32, @indexed collateral : Address, seizedAssets : Uint256, repaidUnits : Uint256, @indexed borrower : Address, postMaturityMode : Bool, receiver : Address, payer : Address, badDebt : Uint256, latestLossFactor : Uint256, latestContinuousFeeCredit : Uint256)
    event ClaimContinuousFee(@indexed caller : Address, @indexed id_ : Bytes32, amount : Uint256, @indexed receiver : Address)

  constants
    ZERO : Uint256 := 0
    ONE : Uint256 := 1
    TWO : Uint256 := 2
    THREE : Uint256 := 3
    FOUR : Uint256 := 4
    FIVE : Uint256 := 5
    SIX : Uint256 := 6
    CBP : Uint256 := 1000000000000
    DEFAULT_TICK_SPACING : Uint256 := 4
    MAX_CONTINUOUS_FEE : Uint256 := 317097919
    MAX_LOSS_FACTOR : Uint256 := 340282366920938463463374607431768211455
    MAX_SETTLEMENT_FEE_0_DAYS : Uint256 := 14000000000000
    MAX_SETTLEMENT_FEE_1_DAY : Uint256 := 14000000000000
    MAX_SETTLEMENT_FEE_7_DAYS : Uint256 := 98000000000000
    MAX_SETTLEMENT_FEE_30_DAYS : Uint256 := 417000000000000
    MAX_SETTLEMENT_FEE_90_DAYS : Uint256 := 1250000000000000
    MAX_SETTLEMENT_FEE_180_DAYS : Uint256 := 2500000000000000
    MAX_SETTLEMENT_FEE_360_DAYS : Uint256 := 5000000000000000
    ONE_DAY : Uint256 := 86400
    SEVEN_DAYS : Uint256 := 604800
    THIRTY_DAYS : Uint256 := 2592000
    NINETY_DAYS : Uint256 := 7776000
    ONE_EIGHTY_DAYS : Uint256 := 15552000
    THREE_SIXTY_DAYS : Uint256 := 31104000
    HUNDRED_YEARS : Uint256 := 3153600000
    WAD : Uint256 := 1000000000000000000
    ORACLE_PRICE_SCALE : Uint256 := 1000000000000000000000000000000000000
    TIME_TO_MAX_LIF : Uint256 := 900
    MAX_COLLATERALS : Uint256 := 128
    MAX_COLLATERALS_PER_BORROWER : Uint256 := 16
    MAX_TICK : Uint256 := 5820
    LIQUIDATION_CURSOR_LOW : Uint256 := 250000000000000000
    LIQUIDATION_CURSOR_HIGH : Uint256 := 500000000000000000
    LLTV_0 : Uint256 := 385000000000000000
    LLTV_1 : Uint256 := 625000000000000000
    LLTV_2 : Uint256 := 770000000000000000
    LLTV_3 : Uint256 := 860000000000000000
    LLTV_4 : Uint256 := 915000000000000000
    LLTV_5 : Uint256 := 945000000000000000
    LLTV_6 : Uint256 := 965000000000000000
    LLTV_7 : Uint256 := 980000000000000000
    LLTV_8 : Uint256 := 1000000000000000000

  constructor () := do
    let cid ← Verity.chainid
    let sender ← msgSender
    setStorage initialChainIdSlot cid
    setStorageAddr roleSetterSlot sender
    setStorageAddr feeSetterSlot sender
    setStorageAddr feeClaimerSlot sender
    setStorageAddr tickSpacingSetterSlot sender

  modifier onlyRoleSetter := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()

  modifier onlyFeeSetter := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    require (sender == currentFeeSetter) "OnlyFeeSetter"

  modifier onlyFeeClaimer := do
    let sender ← msgSender
    let currentFeeClaimer ← getStorageAddr feeClaimerSlot
    require (sender == currentFeeClaimer) "OnlyFeeClaimer"

  modifier onlyTickSpacingSetter := do
    let sender ← msgSender
    let currentTickSpacingSetter ← getStorageAddr tickSpacingSetterSlot
    require (sender == currentTickSpacingSetter) "OnlyTickSpacingSetter"

  function allow_post_interaction_writes multicall (calls : Array Bytes) : Unit := do
    ecmDo multicallDelegateModule []

  function INITIAL_CHAIN_ID () : Uint256 := do
    let cid ← getStorage initialChainIdSlot
    return cid

  function roleSetter () : Address := do
    let value ← getStorageAddr roleSetterSlot
    return value

  function feeSetter () : Address := do
    let value ← getStorageAddr feeSetterSlot
    return value

  function feeClaimer () : Address := do
    let value ← getStorageAddr feeClaimerSlot
    return value

  function tickSpacingSetter () : Address := do
    let value ← getStorageAddr tickSpacingSetterSlot
    return value

  function consumed (user : Address, group : Bytes32) : Uint256 := do
    let value ← structMember2 "consumedSlot" user group "amount"
    return value

  function isAuthorized (authorizer : Address, authorized : Address) : Bool := do
    let value ← structMember2 "isAuthorizedSlot" authorizer authorized "flag"
    return value != ZERO

  function defaultSettlementFeeCbp (loanToken : Address, index : Uint256) : Uint256 := do
    let value ← structMember2 "defaultSettlementFeeSlot" loanToken index "fee"
    return value

  function defaultContinuousFee (loanToken : Address) : Uint256 := do
    let value ← getMapping defaultContinuousFeeSlot loanToken
    return value

  function claimableSettlementFee (token : Address) : Uint256 := do
    let value ← getMapping claimableSettlementFeeSlot token
    return value

  function maxSettlementFee (index : Uint256) : Uint256 := do
    let mut value := MAX_SETTLEMENT_FEE_360_DAYS
    if index == ZERO then
      value := MAX_SETTLEMENT_FEE_0_DAYS
    else
      pure ()
    if index == ONE then
      value := MAX_SETTLEMENT_FEE_1_DAY
    else
      pure ()
    if index == TWO then
      value := MAX_SETTLEMENT_FEE_7_DAYS
    else
      pure ()
    if index == THREE then
      value := MAX_SETTLEMENT_FEE_30_DAYS
    else
      pure ()
    if index == FOUR then
      value := MAX_SETTLEMENT_FEE_90_DAYS
    else
      pure ()
    if index == FIVE then
      value := MAX_SETTLEMENT_FEE_180_DAYS
    else
      pure ()
    return value

  function isLltvAllowed (lltv : Uint256) : Bool := do
    return (lltv == LLTV_0 || lltv == LLTV_1 || lltv == LLTV_2 ||
      lltv == LLTV_3 || lltv == LLTV_4 || lltv == LLTV_5 ||
      lltv == LLTV_6 || lltv == LLTV_7 || lltv == LLTV_8)

  function maxLif (lltv : Uint256, cursor : Uint256) : Uint256 := do
    return mulDivDown WAD WAD (sub WAD (mulDivDown cursor (sub WAD lltv) WAD))

  function min (a : Uint256, b : Uint256) : Uint256 := do
    if a < b then
      return a
    else
      return b

  function validateCollateralParams (collateralParams : Array CollateralParams) : Unit := do
    let collateralCount := arrayLength collateralParams
    requireError (collateralCount > ZERO) NoCollateralParams()
    requireError (collateralCount <= MAX_COLLATERALS) TooManyCollateralParams()
    let mut previousCollateralToken := 0
    forEach "i" collateralCount (do
      let collateralToken := addressToWord (arrayElement collateralParams i).token
      requireError (collateralToken > previousCollateralToken) CollateralParamsNotSorted()
      let lltv := (arrayElement collateralParams i).lltv
      let allowed ← isLltvAllowed lltv
      requireError allowed LltvNotAllowed()
      let lowMaxLif ← maxLif lltv LIQUIDATION_CURSOR_LOW
      let highMaxLif ← maxLif lltv LIQUIDATION_CURSOR_HIGH
      let lif := (arrayElement collateralParams i).maxLif
      requireError (lif == lowMaxLif || lif == highMaxLif) InvalidMaxLif()
      previousCollateralToken := collateralToken)

  function countBits128 (bitmap : Uint256) : Uint256 := do
    let mut count := ZERO
    forEach "i" MAX_COLLATERALS (do
      let mask := shl i ONE
      if bitAnd bitmap mask != ZERO then
        count := add count ONE
      else
        pure ())
    return count

  function toId (market : Market) : Bytes32 := do
    let initialChainId ← getStorage initialChainIdSlot
    let self ← contractAddress
    let id ← ecmCall (fun resultVar => marketIdModule resultVar)
      [initialChainId, addressToWord self]
    return id

  function toMarket (id : Bytes32) : Unit := do
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    ecmDo marketReturnFromCodeModule [id]

  function position (id : Bytes32, user : Address) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256] := do
    let credit ← structMember2 "positionSlot" id user "credit"
    let pendingFee ← structMember2 "positionSlot" id user "pendingFee"
    let lastLossFactor ← structMember2 "positionSlot" id user "lastLossFactor"
    let lastAccrual ← structMember2 "positionSlot" id user "lastAccrual"
    let debt ← structMember2 "positionSlot" id user "debt"
    let collateralBitmap ← structMember2 "positionSlot" id user "collateralBitmap"
    return (credit, pendingFee, lastLossFactor, lastAccrual, debt, collateralBitmap)

  function marketState (id : Bytes32) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256,
        Uint256, Uint256, Uint256, Uint256, Uint256, Uint256] := do
    let totalUnits ← structMember "marketStateSlot" id "totalUnits"
    let lossFactor ← structMember "marketStateSlot" id "lossFactor"
    let withdrawable ← structMember "marketStateSlot" id "withdrawable"
    let continuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
    let settlementFeeCbp0 ← structMember "marketStateSlot" id "settlementFeeCbp0"
    let settlementFeeCbp1 ← structMember "marketStateSlot" id "settlementFeeCbp1"
    let settlementFeeCbp2 ← structMember "marketStateSlot" id "settlementFeeCbp2"
    let settlementFeeCbp3 ← structMember "marketStateSlot" id "settlementFeeCbp3"
    let settlementFeeCbp4 ← structMember "marketStateSlot" id "settlementFeeCbp4"
    let settlementFeeCbp5 ← structMember "marketStateSlot" id "settlementFeeCbp5"
    let settlementFeeCbp6 ← structMember "marketStateSlot" id "settlementFeeCbp6"
    let continuousFee ← structMember "marketStateSlot" id "continuousFee"
    let tickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    return (totalUnits, lossFactor, withdrawable, continuousFeeCredit,
      settlementFeeCbp0, settlementFeeCbp1, settlementFeeCbp2, settlementFeeCbp3,
      settlementFeeCbp4, settlementFeeCbp5, settlementFeeCbp6, continuousFee,
      tickSpacing)

  function updatePositionView (market : Market, id : Bytes32, user : Address) :
      Tuple [Uint256, Uint256, Uint256] := do
    let credit ← structMember2 "positionSlot" id user "credit"
    let pendingFee ← structMember2 "positionSlot" id user "pendingFee"
    let lastLossFactor ← structMember2 "positionSlot" id user "lastLossFactor"
    let lastAccrual ← structMember2 "positionSlot" id user "lastAccrual"
    let marketLossFactor ← structMember "marketStateSlot" id "lossFactor"
    let now ← blockTimestamp
    let mut postSlashCredit := ZERO
    if lastLossFactor < MAX_LOSS_FACTOR then
      postSlashCredit := mulDivDown credit
        (sub MAX_LOSS_FACTOR marketLossFactor)
        (sub MAX_LOSS_FACTOR lastLossFactor)
    else
      pure ()
    let mut postSlashPendingFee := ZERO
    if credit > ZERO then
      postSlashPendingFee := sub pendingFee
        (mulDivUp pendingFee (sub credit postSlashCredit) credit)
    else
      pure ()
    let mut accrualEnd := market.maturity
    if now <= market.maturity then
      accrualEnd := now
    else
      pure ()
    let mut accrued := ZERO
    if lastAccrual < market.maturity then
      accrued := mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub market.maturity lastAccrual)
    else
      pure ()
    return (sub postSlashCredit accrued, sub postSlashPendingFee accrued, accrued)

  function allow_post_interaction_writes updatePosition (market : Market, user : Address) :
      Tuple [Uint256, Uint256, Uint256] := do
    let id ← toId market
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    let credit ← structMember2 "positionSlot" id user "credit"
    let pendingFee ← structMember2 "positionSlot" id user "pendingFee"
    let lastLossFactor ← structMember2 "positionSlot" id user "lastLossFactor"
    let lastAccrual ← structMember2 "positionSlot" id user "lastAccrual"
    let marketLossFactor ← structMember "marketStateSlot" id "lossFactor"
    let now ← blockTimestamp
    let mut postSlashCredit := ZERO
    if lastLossFactor < MAX_LOSS_FACTOR then
      postSlashCredit := mulDivDown credit
        (sub MAX_LOSS_FACTOR marketLossFactor)
        (sub MAX_LOSS_FACTOR lastLossFactor)
    else
      pure ()
    let mut postSlashPendingFee := ZERO
    if credit > ZERO then
      postSlashPendingFee := sub pendingFee
        (mulDivUp pendingFee (sub credit postSlashCredit) credit)
    else
      pure ()
    let mut accrualEnd := market.maturity
    if now <= market.maturity then
      accrualEnd := now
    else
      pure ()
    let mut accrued := ZERO
    if lastAccrual < market.maturity then
      accrued := mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub market.maturity lastAccrual)
    else
      pure ()
    let newCredit := sub postSlashCredit accrued
    let newPendingFee := sub postSlashPendingFee accrued
    let creditDecrease := sub credit newCredit
    let pendingFeeDecrease := sub pendingFee newPendingFee
    setStructMember2 "positionSlot" id user "credit" newCredit
    setStructMember2 "positionSlot" id user "lastLossFactor" marketLossFactor
    setStructMember2 "positionSlot" id user "pendingFee" newPendingFee
    setStructMember2 "positionSlot" id user "lastAccrual" now
    let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
    setStructMember "marketStateSlot" id "continuousFeeCredit"
      (add currentContinuousFeeCredit accrued)
    emit "UpdatePosition"
      [id, user, creditDecrease, pendingFeeDecrease, accrued]
    return (newCredit, newPendingFee, accrued)

  function setRoleSetter (newRoleSetter : Address) : Unit := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()
    setStorageAddr roleSetterSlot newRoleSetter

  function setFeeSetter (newFeeSetter : Address) : Unit := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()
    setStorageAddr feeSetterSlot newFeeSetter

  function setFeeClaimer (newFeeClaimer : Address) : Unit := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()
    setStorageAddr feeClaimerSlot newFeeClaimer

  function setTickSpacingSetter (newTickSpacingSetter : Address) : Unit := do
    let sender ← msgSender
    let currentRoleSetter ← getStorageAddr roleSetterSlot
    requireError (sender == currentRoleSetter) OnlyRoleSetter()
    setStorageAddr tickSpacingSetterSlot newTickSpacingSetter

  function setIsAuthorized (authorized : Address, newIsAuthorized : Bool, onBehalf : Address) :
      Unit := do
    let sender ← msgSender
    let currentAuth ← structMember2 "isAuthorizedSlot" onBehalf sender "flag"
    requireError (sender == onBehalf || currentAuth != ZERO) Unauthorized()
    let mut flag := ZERO
    if newIsAuthorized then
      flag := 1
    else
      flag := ZERO
    setStructMember2 "isAuthorizedSlot" onBehalf authorized "flag" flag

  function setConsumed (group : Bytes32, amount : Uint256, onBehalf : Address) : Unit := do
    let sender ← msgSender
    let currentAuth ← structMember2 "isAuthorizedSlot" onBehalf sender "flag"
    requireError (sender == onBehalf || currentAuth != ZERO) Unauthorized()
    let current ← structMember2 "consumedSlot" onBehalf group "amount"
    requireError (amount >= current) AlreadyConsumed()
    setStructMember2 "consumedSlot" onBehalf group "amount" amount

  function setDefaultSettlementFee
      (loanToken : Address, index : Uint256, newSettlementFee : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    requireError (sender == currentFeeSetter) OnlyFeeSetter()
    requireError (index < 7) InvalidFeeIndex()
    let mut maxFee := MAX_SETTLEMENT_FEE_360_DAYS
    if index == ZERO then
      maxFee := MAX_SETTLEMENT_FEE_0_DAYS
    else
      pure ()
    if index == ONE then
      maxFee := MAX_SETTLEMENT_FEE_1_DAY
    else
      pure ()
    if index == TWO then
      maxFee := MAX_SETTLEMENT_FEE_7_DAYS
    else
      pure ()
    if index == THREE then
      maxFee := MAX_SETTLEMENT_FEE_30_DAYS
    else
      pure ()
    if index == FOUR then
      maxFee := MAX_SETTLEMENT_FEE_90_DAYS
    else
      pure ()
    if index == FIVE then
      maxFee := MAX_SETTLEMENT_FEE_180_DAYS
    else
      pure ()
    requireError (newSettlementFee <= maxFee) SettlementFeeTooHigh()
    requireError (mod newSettlementFee CBP == ZERO) FeeNotMultipleOfFeeCbp()
    setStructMember2 "defaultSettlementFeeSlot" loanToken index "fee" (div newSettlementFee CBP)

  function setDefaultContinuousFee (loanToken : Address, newContinuousFee : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    requireError (sender == currentFeeSetter) OnlyFeeSetter()
    requireError (newContinuousFee <= MAX_CONTINUOUS_FEE) ContinuousFeeTooHigh()
    setMapping defaultContinuousFeeSlot loanToken newContinuousFee

  function claimSettlementFee (token : Address, amount : Uint256, receiver : Address)
      : Unit := do
    let sender ← msgSender
    let currentFeeClaimer ← getStorageAddr feeClaimerSlot
    requireError (sender == currentFeeClaimer) OnlyFeeClaimer()
    let claimable ← getMapping claimableSettlementFeeSlot token
    requireError (amount <= claimable) ConsumedAssets()
    setMapping claimableSettlementFeeSlot token (sub claimable amount)
    safeTransfer token receiver amount

  function allow_post_interaction_writes claimContinuousFee
      (market : Market, amount : Uint256, receiver : Address)
      : Unit := do
    let id ← toId market
    let sender ← msgSender
    let currentFeeClaimer ← getStorageAddr feeClaimerSlot
    requireError (sender == currentFeeClaimer) OnlyFeeClaimer()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
    setStructMember "marketStateSlot" id "continuousFeeCredit"
      (sub currentContinuousFeeCredit amount)
    let currentTotalUnits ← structMember "marketStateSlot" id "totalUnits"
    setStructMember "marketStateSlot" id "totalUnits" (sub currentTotalUnits amount)
    let currentWithdrawable ← structMember "marketStateSlot" id "withdrawable"
    setStructMember "marketStateSlot" id "withdrawable" (sub currentWithdrawable amount)
    emit "ClaimContinuousFee" [sender, id, amount, receiver]
    safeTransfer market.loanToken receiver amount

  function setMarketTickSpacing (id : Bytes32, newTickSpacing : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentTickSpacingSetter ← getStorageAddr tickSpacingSetterSlot
    requireError (sender == currentTickSpacingSetter) OnlyTickSpacingSetter()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    requireError (newTickSpacing > ZERO) InvalidTickSpacing()
    requireError (mod currentTickSpacing newTickSpacing == ZERO) InvalidTickSpacing()
    setStructMember "marketStateSlot" id "tickSpacing" newTickSpacing

  function setMarketSettlementFee
      (id : Bytes32, index : Uint256, newSettlementFee : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    requireError (sender == currentFeeSetter) OnlyFeeSetter()
    requireError (index < 7) InvalidFeeIndex()
    let mut maxFee := MAX_SETTLEMENT_FEE_360_DAYS
    if index == ZERO then
      maxFee := MAX_SETTLEMENT_FEE_0_DAYS
    else
      pure ()
    if index == ONE then
      maxFee := MAX_SETTLEMENT_FEE_1_DAY
    else
      pure ()
    if index == TWO then
      maxFee := MAX_SETTLEMENT_FEE_7_DAYS
    else
      pure ()
    if index == THREE then
      maxFee := MAX_SETTLEMENT_FEE_30_DAYS
    else
      pure ()
    if index == FOUR then
      maxFee := MAX_SETTLEMENT_FEE_90_DAYS
    else
      pure ()
    if index == FIVE then
      maxFee := MAX_SETTLEMENT_FEE_180_DAYS
    else
      pure ()
    requireError (newSettlementFee <= maxFee) SettlementFeeTooHigh()
    requireError (mod newSettlementFee CBP == ZERO) FeeNotMultipleOfFeeCbp()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    let newSettlementFeeCbp := div newSettlementFee CBP
    if index == ZERO then
      setStructMember "marketStateSlot" id "settlementFeeCbp0" newSettlementFeeCbp
    else
      pure ()
    if index == ONE then
      setStructMember "marketStateSlot" id "settlementFeeCbp1" newSettlementFeeCbp
    else
      pure ()
    if index == TWO then
      setStructMember "marketStateSlot" id "settlementFeeCbp2" newSettlementFeeCbp
    else
      pure ()
    if index == THREE then
      setStructMember "marketStateSlot" id "settlementFeeCbp3" newSettlementFeeCbp
    else
      pure ()
    if index == FOUR then
      setStructMember "marketStateSlot" id "settlementFeeCbp4" newSettlementFeeCbp
    else
      pure ()
    if index == FIVE then
      setStructMember "marketStateSlot" id "settlementFeeCbp5" newSettlementFeeCbp
    else
      pure ()
    if index == SIX then
      setStructMember "marketStateSlot" id "settlementFeeCbp6" newSettlementFeeCbp
    else
      pure ()

  function setMarketContinuousFee (id : Bytes32, newContinuousFee : Uint256)
      : Unit := do
    let sender ← msgSender
    let currentFeeSetter ← getStorageAddr feeSetterSlot
    requireError (sender == currentFeeSetter) OnlyFeeSetter()
    requireError (newContinuousFee <= MAX_CONTINUOUS_FEE) ContinuousFeeTooHigh()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    setStructMember "marketStateSlot" id "continuousFee" newContinuousFee

  function allow_post_interaction_writes touchMarket (market : Market) : Bytes32 := do
    let id ← toId market
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    if currentTickSpacing == ZERO then
      let now ← blockTimestamp
      requireError (market.maturity <= add now HUNDRED_YEARS) MaturityTooFar()
      validateCollateralParams market.collateralParams
      let salt ← getStorage initialChainIdSlot
      let _marketPointer ← ecmCall (fun resultVar => storeMarketInCodeModule resultVar)
        [salt]
      setStructMember "marketStateSlot" id "tickSpacing" DEFAULT_TICK_SPACING
      let settlementFeeCbp0 ← defaultSettlementFeeCbp market.loanToken ZERO
      let settlementFeeCbp1 ← defaultSettlementFeeCbp market.loanToken ONE
      let settlementFeeCbp2 ← defaultSettlementFeeCbp market.loanToken TWO
      let settlementFeeCbp3 ← defaultSettlementFeeCbp market.loanToken THREE
      let settlementFeeCbp4 ← defaultSettlementFeeCbp market.loanToken FOUR
      let settlementFeeCbp5 ← defaultSettlementFeeCbp market.loanToken FIVE
      let settlementFeeCbp6 ← defaultSettlementFeeCbp market.loanToken SIX
      let continuous ← getMapping defaultContinuousFeeSlot market.loanToken
      setStructMember "marketStateSlot" id "settlementFeeCbp0" settlementFeeCbp0
      setStructMember "marketStateSlot" id "settlementFeeCbp1" settlementFeeCbp1
      setStructMember "marketStateSlot" id "settlementFeeCbp2" settlementFeeCbp2
      setStructMember "marketStateSlot" id "settlementFeeCbp3" settlementFeeCbp3
      setStructMember "marketStateSlot" id "settlementFeeCbp4" settlementFeeCbp4
      setStructMember "marketStateSlot" id "settlementFeeCbp5" settlementFeeCbp5
      setStructMember "marketStateSlot" id "settlementFeeCbp6" settlementFeeCbp6
      setStructMember "marketStateSlot" id "continuousFee" continuous
    else
      pure ()
    return id

  function settlementFee (id : Bytes32, timeToMaturity : Uint256) : Uint256 := do
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (currentTickSpacing > ZERO) MarketNotCreated()
    let settlementFeeCbp0 ← structMember "marketStateSlot" id "settlementFeeCbp0"
    let settlementFeeCbp1 ← structMember "marketStateSlot" id "settlementFeeCbp1"
    let settlementFeeCbp2 ← structMember "marketStateSlot" id "settlementFeeCbp2"
    let settlementFeeCbp3 ← structMember "marketStateSlot" id "settlementFeeCbp3"
    let settlementFeeCbp4 ← structMember "marketStateSlot" id "settlementFeeCbp4"
    let settlementFeeCbp5 ← structMember "marketStateSlot" id "settlementFeeCbp5"
    let settlementFeeCbp6 ← structMember "marketStateSlot" id "settlementFeeCbp6"
    let mut start := ONE_EIGHTY_DAYS
    let mut finish := THREE_SIXTY_DAYS
    let mut feeLower := mul settlementFeeCbp5 CBP
    let mut feeUpper := mul settlementFeeCbp6 CBP
    if timeToMaturity < ONE_DAY then
      start := ZERO
      finish := ONE_DAY
      feeLower := mul settlementFeeCbp0 CBP
      feeUpper := mul settlementFeeCbp1 CBP
    else
      pure ()
    if timeToMaturity >= ONE_DAY && timeToMaturity < SEVEN_DAYS then
      start := ONE_DAY
      finish := SEVEN_DAYS
      feeLower := mul settlementFeeCbp1 CBP
      feeUpper := mul settlementFeeCbp2 CBP
    else
      pure ()
    if timeToMaturity >= SEVEN_DAYS && timeToMaturity < THIRTY_DAYS then
      start := SEVEN_DAYS
      finish := THIRTY_DAYS
      feeLower := mul settlementFeeCbp2 CBP
      feeUpper := mul settlementFeeCbp3 CBP
    else
      pure ()
    if timeToMaturity >= THIRTY_DAYS && timeToMaturity < NINETY_DAYS then
      start := THIRTY_DAYS
      finish := NINETY_DAYS
      feeLower := mul settlementFeeCbp3 CBP
      feeUpper := mul settlementFeeCbp4 CBP
    else
      pure ()
    if timeToMaturity >= NINETY_DAYS && timeToMaturity < ONE_EIGHTY_DAYS then
      start := NINETY_DAYS
      finish := ONE_EIGHTY_DAYS
      feeLower := mul settlementFeeCbp4 CBP
      feeUpper := mul settlementFeeCbp5 CBP
    else
      pure ()
    if timeToMaturity >= THREE_SIXTY_DAYS then
      return mul settlementFeeCbp6 CBP
    else
      return div
        (add
          (mul feeLower (sub finish timeToMaturity))
          (mul feeUpper (sub timeToMaturity start)))
        (sub finish start)

  function tickToPrice (tick : Uint256) : Uint256 := do
    requireError (tick <= 5820) TickOutOfRange()
    let price ← ecmCall (fun resultVar => tickToPriceModule resultVar) [tick]
    return price

  function enterGateCanIncreaseCredit (gate : Address, account : Address)
      : Uint256 := do
    let allowed ← ecmCall
      (fun resultVar => Compiler.Modules.Calls.withReturnModule resultVar 0x58ac9f9e 1 true)
      [gate, addressToWord account]
    return allowed

  function enterGateCanIncreaseDebt (gate : Address, account : Address)
      : Uint256 := do
    let allowed ← ecmCall
      (fun resultVar => Compiler.Modules.Calls.withReturnModule resultVar 0xfe9bf956 1 true)
      [gate, addressToWord account]
    return allowed

  function allow_post_interaction_writes updateBuyerForTake
      (id : Bytes32, buyer : Address, units : Uint256, maturity : Uint256,
        now : Uint256) : Uint256 := do
    let buyerDebt ← structMember2 "positionSlot" id buyer "debt"
    let buyerDebtDecrease ← min units buyerDebt
    let buyerCreditIncrease := sub units buyerDebtDecrease
    let continuousFeeValue ← structMember "marketStateSlot" id "continuousFee"
    let mut timeToMaturity := ZERO
    if maturity > now then
      timeToMaturity := sub maturity now
    else
      pure ()
    let buyerPendingFeeIncrease :=
      mulDivDown buyerCreditIncrease (mul continuousFeeValue timeToMaturity) WAD
    let buyerCredit ← structMember2 "positionSlot" id buyer "credit"
    let buyerPendingFee ← structMember2 "positionSlot" id buyer "pendingFee"
    let buyerLastLossFactor ← structMember2 "positionSlot" id buyer "lastLossFactor"
    let buyerLastAccrual ← structMember2 "positionSlot" id buyer "lastAccrual"
    let buyerMarketLossFactor ← structMember "marketStateSlot" id "lossFactor"
    let mut buyerPostSlashCredit := ZERO
    if buyerLastLossFactor < MAX_LOSS_FACTOR then
      buyerPostSlashCredit := mulDivDown buyerCredit
        (sub MAX_LOSS_FACTOR buyerMarketLossFactor)
        (sub MAX_LOSS_FACTOR buyerLastLossFactor)
    else
      pure ()
    let mut buyerPostSlashPendingFee := ZERO
    if buyerCredit > ZERO then
      buyerPostSlashPendingFee := sub buyerPendingFee
        (mulDivUp buyerPendingFee (sub buyerCredit buyerPostSlashCredit) buyerCredit)
    else
      pure ()
    let mut buyerAccrualEnd := maturity
    if now <= maturity then
      buyerAccrualEnd := now
    else
      pure ()
    let mut buyerAccrued := ZERO
    if buyerLastAccrual < maturity then
      buyerAccrued := mulDivDown buyerPostSlashPendingFee
        (sub buyerAccrualEnd buyerLastAccrual)
        (sub maturity buyerLastAccrual)
    else
      pure ()
    let buyerCreditAfterUpdate := sub buyerPostSlashCredit buyerAccrued
    let buyerPendingFeeAfterUpdate := sub buyerPostSlashPendingFee buyerAccrued
    let buyerCreditDecrease := sub buyerCredit buyerCreditAfterUpdate
    let buyerPendingFeeDecreaseForUpdate := sub buyerPendingFee buyerPendingFeeAfterUpdate
    if buyerCredit > ZERO || buyerCreditIncrease > ZERO then
      setStructMember2 "positionSlot" id buyer "credit" buyerCreditAfterUpdate
      setStructMember2 "positionSlot" id buyer "lastLossFactor" buyerMarketLossFactor
      setStructMember2 "positionSlot" id buyer "pendingFee" buyerPendingFeeAfterUpdate
      setStructMember2 "positionSlot" id buyer "lastAccrual" now
      let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
      setStructMember "marketStateSlot" id "continuousFeeCredit"
        (add currentContinuousFeeCredit buyerAccrued)
    else
      pure ()
    emit "UpdatePosition"
      [id, buyer, buyerCreditDecrease, buyerPendingFeeDecreaseForUpdate, buyerAccrued]
    setStructMember2 "positionSlot" id buyer "debt" (sub buyerDebt buyerDebtDecrease)
    setStructMember2 "positionSlot" id buyer "pendingFee"
      (add buyerPendingFeeAfterUpdate buyerPendingFeeIncrease)
    setStructMember2 "positionSlot" id buyer "credit"
      (add buyerCreditAfterUpdate buyerCreditIncrease)
    return buyerCreditIncrease

  function allow_post_interaction_writes updateSellerForTake
      (id : Bytes32, seller : Address, units : Uint256, maturity : Uint256,
        now : Uint256) : Tuple [Uint256, Uint256, Uint256] := do
    let sellerCredit ← structMember2 "positionSlot" id seller "credit"
    let sellerPendingFee ← structMember2 "positionSlot" id seller "pendingFee"
    let sellerLastLossFactor ← structMember2 "positionSlot" id seller "lastLossFactor"
    let sellerLastAccrual ← structMember2 "positionSlot" id seller "lastAccrual"
    let sellerMarketLossFactor ← structMember "marketStateSlot" id "lossFactor"
    let mut sellerPostSlashCredit := ZERO
    if sellerLastLossFactor < MAX_LOSS_FACTOR then
      sellerPostSlashCredit := mulDivDown sellerCredit
        (sub MAX_LOSS_FACTOR sellerMarketLossFactor)
        (sub MAX_LOSS_FACTOR sellerLastLossFactor)
    else
      pure ()
    let mut sellerPostSlashPendingFee := ZERO
    if sellerCredit > ZERO then
      sellerPostSlashPendingFee := sub sellerPendingFee
        (mulDivUp sellerPendingFee (sub sellerCredit sellerPostSlashCredit)
          sellerCredit)
    else
      pure ()
    let mut sellerAccrualEnd := maturity
    if now <= maturity then
      sellerAccrualEnd := now
    else
      pure ()
    let mut sellerAccrued := ZERO
    if sellerLastAccrual < maturity then
      sellerAccrued := mulDivDown sellerPostSlashPendingFee
        (sub sellerAccrualEnd sellerLastAccrual)
        (sub maturity sellerLastAccrual)
    else
      pure ()
    let sellerCreditAfterUpdate := sub sellerPostSlashCredit sellerAccrued
    let sellerPendingFeeAfterUpdate := sub sellerPostSlashPendingFee sellerAccrued
    let sellerCreditDecreaseForUpdate := sub sellerCredit sellerCreditAfterUpdate
    let sellerPendingFeeDecreaseForUpdate :=
      sub sellerPendingFee sellerPendingFeeAfterUpdate
    if sellerCredit > ZERO then
      setStructMember2 "positionSlot" id seller "credit" sellerCreditAfterUpdate
      setStructMember2 "positionSlot" id seller "lastLossFactor" sellerMarketLossFactor
      setStructMember2 "positionSlot" id seller "pendingFee" sellerPendingFeeAfterUpdate
      setStructMember2 "positionSlot" id seller "lastAccrual" now
      let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
      setStructMember "marketStateSlot" id "continuousFeeCredit"
        (add currentContinuousFeeCredit sellerAccrued)
    else
      pure ()
    emit "UpdatePosition"
      [id, seller, sellerCreditDecreaseForUpdate, sellerPendingFeeDecreaseForUpdate,
        sellerAccrued]
    let sellerCreditDecrease ← min units sellerCreditAfterUpdate
    let sellerDebtIncrease := sub units sellerCreditDecrease
    let sellerDebt ← structMember2 "positionSlot" id seller "debt"
    let mut sellerPendingFeeDecrease := ZERO
    if sellerCreditAfterUpdate > ZERO then
      sellerPendingFeeDecrease :=
        mulDivUp sellerPendingFeeAfterUpdate sellerCreditDecrease sellerCreditAfterUpdate
    else
      pure ()
    setStructMember2 "positionSlot" id seller "pendingFee"
      (sub sellerPendingFeeAfterUpdate sellerPendingFeeDecrease)
    setStructMember2 "positionSlot" id seller "credit"
      (sub sellerCreditAfterUpdate sellerCreditDecrease)
    setStructMember2 "positionSlot" id seller "debt" (add sellerDebt sellerDebtIncrease)
    return (sellerCreditDecrease, sellerDebtIncrease, sellerPendingFeeDecrease)

  function takeAssetsForPrice
      (id : Bytes32, units : Uint256, tick : Uint256, offerIsBuy : Bool,
        maturity : Uint256, now : Uint256, buyerCreditIncrease : Uint256) :
      Tuple [Uint256, Uint256, Uint256] := do
    let offerPrice ← tickToPrice tick
    let mut timeToMaturityForFee := ZERO
    if maturity > now then
      timeToMaturityForFee := sub maturity now
    else
      pure ()
    let settlementFeeValue ← settlementFee id timeToMaturityForFee
    let continuousFeeValueForCallback ← structMember "marketStateSlot" id "continuousFee"
    let buyerPendingFeeIncrease :=
      mulDivDown buyerCreditIncrease
        (mul continuousFeeValueForCallback timeToMaturityForFee) WAD
    let mut sellerPrice := offerPrice
    if offerIsBuy then
      sellerPrice := sub offerPrice settlementFeeValue
    else
      pure ()
    let buyerPrice := add sellerPrice settlementFeeValue
    let mut buyerAssets := ZERO
    let mut sellerAssets := ZERO
    if offerIsBuy then
      buyerAssets := mulDivDown units buyerPrice WAD
      sellerAssets := mulDivDown units sellerPrice WAD
    else
      buyerAssets := mulDivUp units buyerPrice WAD
      sellerAssets := mulDivUp units sellerPrice WAD
    return (buyerAssets, sellerAssets, buyerPendingFeeIncrease)

  function allow_post_interaction_writes take
      (offer : Offer, ratifierData : Bytes, units : Uint256, taker : Address,
        receiverIfTakerIsSeller : Address, takerCallback : Address,
        takerCallbackData : Bytes)
      local_obligations [take_calldata_market_scalars := assumed
        "Temporary Midnight scaffold reads loanToken and maturity from calldata because nested dynamic struct projection is not yet supported by the Verity contract macro."] :
      Tuple [Uint256, Uint256] := do
    let sender ← msgSender
    let authorized ← isAuthorized taker sender
    requireError (taker == sender || authorized) TakerUnauthorized()
    let offerBase := add (calldataload 4) 4
    let marketBase := add offerBase (calldataload offerBase)
    let loanToken := wordToAddress (calldataload marketBase)
    let maturity := calldataload (add marketBase 64)
    let enterGate := wordToAddress (calldataload (add marketBase 128))
    let initialChainId ← getStorage initialChainIdSlot
    let contractSelf ← contractAddress
    let id ← ecmCall (fun resultVar => marketIdAtOffsetModule resultVar)
      [initialChainId, addressToWord contractSelf, marketBase]
    let currentMarketTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    if currentMarketTickSpacing == ZERO then
      let now ← blockTimestamp
      requireError (maturity <= add now HUNDRED_YEARS) MaturityTooFar()
      setStructMember "marketStateSlot" id "tickSpacing" DEFAULT_TICK_SPACING
      let settlementFeeCbp0 ← defaultSettlementFeeCbp loanToken ZERO
      let settlementFeeCbp1 ← defaultSettlementFeeCbp loanToken ONE
      let settlementFeeCbp2 ← defaultSettlementFeeCbp loanToken TWO
      let settlementFeeCbp3 ← defaultSettlementFeeCbp loanToken THREE
      let settlementFeeCbp4 ← defaultSettlementFeeCbp loanToken FOUR
      let settlementFeeCbp5 ← defaultSettlementFeeCbp loanToken FIVE
      let settlementFeeCbp6 ← defaultSettlementFeeCbp loanToken SIX
      let continuous ← getMapping defaultContinuousFeeSlot loanToken
      setStructMember "marketStateSlot" id "settlementFeeCbp0" settlementFeeCbp0
      setStructMember "marketStateSlot" id "settlementFeeCbp1" settlementFeeCbp1
      setStructMember "marketStateSlot" id "settlementFeeCbp2" settlementFeeCbp2
      setStructMember "marketStateSlot" id "settlementFeeCbp3" settlementFeeCbp3
      setStructMember "marketStateSlot" id "settlementFeeCbp4" settlementFeeCbp4
      setStructMember "marketStateSlot" id "settlementFeeCbp5" settlementFeeCbp5
      setStructMember "marketStateSlot" id "settlementFeeCbp6" settlementFeeCbp6
      setStructMember "marketStateSlot" id "continuousFee" continuous
    else
      pure ()
    let lossFactorForGuard ← structMember "marketStateSlot" id "lossFactor"
    requireError (lossFactorForGuard < MAX_LOSS_FACTOR)
      MarketLossFactorMaxedOut()
    requireError (offer.maxAssets == ZERO || offer.maxUnits == ZERO) MultipleNonZero()
    let currentTickSpacing ← structMember "marketStateSlot" id "tickSpacing"
    requireError (mod offer.tick currentTickSpacing == ZERO) TickNotAccessible()
    let now ← blockTimestamp
    requireError (now >= offer.start) OfferNotStarted()
    requireError (now <= offer.expiry) OfferExpired()
    requireError (offer.maker != taker) SelfTake()
    let ratifierAuthorized ← isAuthorized offer.maker offer.ratifier
    requireError ratifierAuthorized RatifierUnauthorized()
    ecmDo ratifierCallbackModule [addressToWord offer.ratifier]
    requireError (units <= MAX_LOSS_FACTOR) CastOverflow()

    let mut newConsumed := ZERO
    if offer.maxAssets > ZERO then
      let offerPriceConsumed ← tickToPrice offer.tick
      let mut timeToMaturityConsumed := ZERO
      if maturity > now then
        timeToMaturityConsumed := sub maturity now
      else
        pure ()
      let settlementFeeConsumed ← settlementFee id timeToMaturityConsumed
      let mut sellerPriceConsumed := offerPriceConsumed
      if offer.buy then
        sellerPriceConsumed := sub offerPriceConsumed settlementFeeConsumed
      else
        pure ()
      let buyerPriceConsumed := add sellerPriceConsumed settlementFeeConsumed
      let mut buyerAssetsConsumed := ZERO
      let mut sellerAssetsConsumed := ZERO
      if offer.buy then
        buyerAssetsConsumed := mulDivDown units buyerPriceConsumed WAD
        sellerAssetsConsumed := mulDivDown units sellerPriceConsumed WAD
      else
        buyerAssetsConsumed := mulDivUp units buyerPriceConsumed WAD
        sellerAssetsConsumed := mulDivUp units sellerPriceConsumed WAD
      requireError (buyerAssetsConsumed >= sellerAssetsConsumed) InconsistentInput()
      let currentConsumed ← structMember2 "consumedSlot" offer.maker offer.group "amount"
      let mut consumedIncrease := sellerAssetsConsumed
      if offer.buy then
        consumedIncrease := buyerAssetsConsumed
      else
        pure ()
      newConsumed := add currentConsumed consumedIncrease
      requireError (newConsumed <= offer.maxAssets) ConsumedAssets()
      setStructMember2 "consumedSlot" offer.maker offer.group "amount" newConsumed
    else
      let currentConsumed ← structMember2 "consumedSlot" offer.maker offer.group "amount"
      newConsumed := add currentConsumed units
      requireError (newConsumed <= offer.maxUnits) ConsumedUnits()
      setStructMember2 "consumedSlot" offer.maker offer.group "amount" newConsumed

    let mut buyer := taker
    let mut seller := offer.maker
    if offer.buy then
      buyer := offer.maker
      seller := taker
    else
      pure ()

    let buyerCreditIncrease ← updateBuyerForTake id buyer units maturity now
    let (sellerCreditDecrease, sellerDebtIncrease, sellerPendingFeeDecrease) ←
      updateSellerForTake id seller units maturity now
    requireError (now <= maturity || sellerDebtIncrease == ZERO)
      CannotIncreaseDebtPostMaturity()
    let mut reduceOnlyAllowed := true
    if offer.reduceOnly then
      if offer.buy then
        reduceOnlyAllowed := buyerCreditIncrease == ZERO
      else
        reduceOnlyAllowed := sellerDebtIncrease == ZERO
    else
      pure ()
    requireError reduceOnlyAllowed MakerCreditOrDebtIncreased()

    let mut buyerGateAllowed := true
    if enterGate != 0 then
      if buyerCreditIncrease > ZERO then
        let canIncreaseCredit ← enterGateCanIncreaseCredit enterGate buyer
        buyerGateAllowed := canIncreaseCredit != ZERO
      else
        pure ()
    else
      pure ()
    requireError buyerGateAllowed BuyerGatedFromIncreasingCredit()

    let mut sellerGateAllowed := true
    if enterGate != 0 then
      if sellerDebtIncrease > ZERO then
        let canIncreaseDebt ← enterGateCanIncreaseDebt enterGate seller
        sellerGateAllowed := canIncreaseDebt != ZERO
      else
        pure ()
    else
      pure ()
    requireError sellerGateAllowed SellerGatedFromIncreasingDebt()

    let currentTotalUnits ← structMember "marketStateSlot" id "totalUnits"
    let mut newTotalUnits := add currentTotalUnits buyerCreditIncrease
    newTotalUnits := sub newTotalUnits sellerCreditDecrease
    setStructMember "marketStateSlot" id "totalUnits" newTotalUnits

    let (buyerAssets, sellerAssets, buyerPendingFeeIncrease) ←
      takeAssetsForPrice id units offer.tick offer.buy maturity now buyerCreditIncrease
    requireError (buyerAssets >= sellerAssets) InconsistentInput()
    let claimableBefore ← getMapping claimableSettlementFeeSlot loanToken
    setMapping claimableSettlementFeeSlot loanToken
      (add claimableBefore (sub buyerAssets sellerAssets))

    let mut receiver := offer.receiverIfMakerIsSeller
    if offer.buy then
      receiver := receiverIfTakerIsSeller
    else
      pure ()
    let mut payer := buyer
    if offer.buy then
      pure ()
    else
      payer := sender
    let mut buyerCallback := takerCallback
    if offer.buy then
      buyerCallback := offer.callback
    else
      pure ()
    let mut offerIsBuyWord := ZERO
    if offer.buy then
      offerIsBuyWord := ONE
    else
      pure ()
    let takeEventPtr ← ecmCall (fun resultVar => takeEventPrepareAModule resultVar)
      [addressToWord sender, units, offerIsBuyWord, offer.group, buyerAssets, sellerAssets]
    ecmDo takeEventPrepareBModule
      [takeEventPtr, newConsumed, buyerPendingFeeIncrease, sellerPendingFeeDecrease,
        buyerCreditIncrease, sellerCreditDecrease]
    ecmDo takeEventEmitModule
      [takeEventPtr, addressToWord receiver, addressToWord payer, id, addressToWord taker,
        addressToWord offer.maker]
    ecmDo liquidationLockDepthEnterModule [id, addressToWord seller]
    if buyerCallback != 0 then
      payer := buyerCallback
      if offer.buy then
        ecmDo (buyCallbackModule true)
          [addressToWord buyerCallback, id, buyerAssets, units,
            buyerPendingFeeIncrease, addressToWord buyer]
      else
        ecmDo (buyCallbackModule false)
          [addressToWord buyerCallback, id, buyerAssets, units,
            buyerPendingFeeIncrease, addressToWord buyer]
    else
      pure ()
    let feeAssets := sub buyerAssets sellerAssets
    if feeAssets > ZERO then
      let self ← contractAddress
      safeTransferFrom loanToken payer self feeAssets
    else
      pure ()
    if sellerAssets > ZERO then
      safeTransferFrom loanToken payer receiver sellerAssets
    else
      pure ()
    let mut sellerCallback := offer.callback
    if offer.buy then
      sellerCallback := takerCallback
    else
      pure ()
    if sellerCallback != 0 then
      if offer.buy then
        ecmDo (sellCallbackModule false)
          [addressToWord sellerCallback, id, sellerAssets, units,
            sellerPendingFeeDecrease, addressToWord seller,
            addressToWord receiver, ZERO]
      else
        ecmDo (sellCallbackModule true)
          [addressToWord sellerCallback, id, sellerAssets, units,
            sellerPendingFeeDecrease, addressToWord seller,
            addressToWord receiver, ZERO]
    else
      pure ()
    let lockedAfterCallbacks ← ecmCall (fun resultVar => liquidationLockDepthExitGetModule resultVar)
      [id, addressToWord seller]
    if lockedAfterCallbacks != ZERO then
      pure ()
    else
      let sellerHealthy ← ecmCall (fun resultVar => offerMarketIsHealthyModule resultVar)
        [id, addressToWord seller]
      requireError (sellerHealthy != ZERO) SellerIsLiquidatable()
    return (buyerAssets, sellerAssets)

  function creditOf (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "credit"
    return value

  function debtOf (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "debt"
    return value

  function totalUnits (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "totalUnits"
    return value

  function lossFactor (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "lossFactor"
    return value

  function tickSpacing (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "tickSpacing"
    return value

  function settlementFeeCbps (id : Bytes32) :
      Tuple [Uint256, Uint256, Uint256, Uint256, Uint256, Uint256, Uint256] := do
    let settlementFeeCbp0 ← structMember "marketStateSlot" id "settlementFeeCbp0"
    let settlementFeeCbp1 ← structMember "marketStateSlot" id "settlementFeeCbp1"
    let settlementFeeCbp2 ← structMember "marketStateSlot" id "settlementFeeCbp2"
    let settlementFeeCbp3 ← structMember "marketStateSlot" id "settlementFeeCbp3"
    let settlementFeeCbp4 ← structMember "marketStateSlot" id "settlementFeeCbp4"
    let settlementFeeCbp5 ← structMember "marketStateSlot" id "settlementFeeCbp5"
    let settlementFeeCbp6 ← structMember "marketStateSlot" id "settlementFeeCbp6"
    return (settlementFeeCbp0, settlementFeeCbp1, settlementFeeCbp2, settlementFeeCbp3,
      settlementFeeCbp4, settlementFeeCbp5, settlementFeeCbp6)

  function withdrawable (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "withdrawable"
    return value

  function continuousFee (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "continuousFee"
    return value

  function continuousFeeCredit (id : Bytes32) : Uint256 := do
    let value ← structMember "marketStateSlot" id "continuousFeeCredit"
    return value

  function allow_post_interaction_writes withdraw
      (market : Market, units : Uint256, onBehalf : Address,
        receiver : Address) : Unit := do
    let sender ← msgSender
    let authorized ← isAuthorized onBehalf sender
    requireError (onBehalf == sender || authorized) Unauthorized()
    let id ← toId market
    let creditBeforeUpdate ← structMember2 "positionSlot" id onBehalf "credit"
    let pendingFeeBeforeUpdate ← structMember2 "positionSlot" id onBehalf "pendingFee"
    let lastLossFactor ← structMember2 "positionSlot" id onBehalf "lastLossFactor"
    let lastAccrual ← structMember2 "positionSlot" id onBehalf "lastAccrual"
    let marketLossFactor ← structMember "marketStateSlot" id "lossFactor"
    let now ← blockTimestamp
    let mut postSlashCredit := ZERO
    if lastLossFactor < MAX_LOSS_FACTOR then
      postSlashCredit := mulDivDown creditBeforeUpdate
        (sub MAX_LOSS_FACTOR marketLossFactor)
        (sub MAX_LOSS_FACTOR lastLossFactor)
    else
      pure ()
    let mut postSlashPendingFee := ZERO
    if creditBeforeUpdate > ZERO then
      postSlashPendingFee := sub pendingFeeBeforeUpdate
        (mulDivUp pendingFeeBeforeUpdate (sub creditBeforeUpdate postSlashCredit)
          creditBeforeUpdate)
    else
      pure ()
    let mut accrualEnd := market.maturity
    if now <= market.maturity then
      accrualEnd := now
    else
      pure ()
    let mut accrued := ZERO
    if lastAccrual < market.maturity then
      accrued := mulDivDown postSlashPendingFee
        (sub accrualEnd lastAccrual)
        (sub market.maturity lastAccrual)
    else
      pure ()
    let creditAfterUpdate := sub postSlashCredit accrued
    let pendingFeeAfterUpdate := sub postSlashPendingFee accrued
    setStructMember2 "positionSlot" id onBehalf "credit" creditAfterUpdate
    setStructMember2 "positionSlot" id onBehalf "lastLossFactor" marketLossFactor
    setStructMember2 "positionSlot" id onBehalf "pendingFee" pendingFeeAfterUpdate
    setStructMember2 "positionSlot" id onBehalf "lastAccrual" now
    let currentContinuousFeeCredit ← structMember "marketStateSlot" id "continuousFeeCredit"
    setStructMember "marketStateSlot" id "continuousFeeCredit"
      (add currentContinuousFeeCredit accrued)
    let creditDecrease := sub creditBeforeUpdate creditAfterUpdate
    let updatePendingFeeDecrease := sub pendingFeeBeforeUpdate pendingFeeAfterUpdate
    emit "UpdatePosition"
      [id, onBehalf, creditDecrease, updatePendingFeeDecrease, accrued]
    let mut pendingFeeDecrease := ZERO
    if creditAfterUpdate > ZERO then
      pendingFeeDecrease := mulDivUp pendingFeeAfterUpdate units creditAfterUpdate
    else
      pure ()
    setStructMember2 "positionSlot" id onBehalf "pendingFee"
      (sub pendingFeeAfterUpdate pendingFeeDecrease)
    let credit ← structMember2 "positionSlot" id onBehalf "credit"
    setStructMember2 "positionSlot" id onBehalf "credit" (sub credit units)
    let withdrawableAmount ← structMember "marketStateSlot" id "withdrawable"
    setStructMember "marketStateSlot" id "withdrawable" (sub withdrawableAmount units)
    let total ← structMember "marketStateSlot" id "totalUnits"
    setStructMember "marketStateSlot" id "totalUnits" (sub total units)
    emit "Withdraw" [sender, id, units, onBehalf, receiver, pendingFeeDecrease]
    safeTransfer market.loanToken receiver units

  function allow_post_interaction_writes repay
      (market : Market, units : Uint256, onBehalf : Address,
        callback : Address, data : Bytes) : Unit := do
    let sender ← msgSender
    let authorized ← isAuthorized onBehalf sender
    requireError (onBehalf == sender || authorized) Unauthorized()
    let id ← toId market
    let debt ← structMember2 "positionSlot" id onBehalf "debt"
    setStructMember2 "positionSlot" id onBehalf "debt" (sub debt units)
    let withdrawableAmount ← structMember "marketStateSlot" id "withdrawable"
    setStructMember "marketStateSlot" id "withdrawable" (add withdrawableAmount units)
    let mut payer := sender
    if callback != 0 then
      payer := callback
      ecmDo repayCallbackModule
        [addressToWord callback, id, units, addressToWord onBehalf]
    else
      pure ()
    let self ← contractAddress
    safeTransferFrom market.loanToken payer self units

  function collateralTokenAt
      (collateralParams : Array CollateralParams, index : Uint256) : Address := do
    return (arrayElement collateralParams index).token

  function collateralLltvAt
      (collateralParams : Array CollateralParams, index : Uint256) : Uint256 := do
    return (arrayElement collateralParams index).lltv

  function collateralMaxLifAt
      (collateralParams : Array CollateralParams, index : Uint256) : Uint256 := do
    return (arrayElement collateralParams index).maxLif

  function collateralOracleAt
      (collateralParams : Array CollateralParams, index : Uint256) : Address := do
    return (arrayElement collateralParams index).oracle

  function oraclePrice (oracle : Address)
      : Uint256 := do
    let price ← ecmCall
      (fun resultVar => Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
      [oracle]
    return price

  function liquidatorGateCanLiquidate (gate : Address, account : Address)
      : Uint256 := do
    let allowed ← ecmCall
      (fun resultVar => Compiler.Modules.Calls.withReturnModule resultVar 0xb9f4ff55 1 true)
      [gate, addressToWord account]
    return allowed

  function liquidatorGateCanLiquidateOrDefault (gate : Address, account : Address)
      : Uint256 := do
    let mut allowed := ONE
    if gate != 0 then
      let loaded ← liquidatorGateCanLiquidate gate account
      allowed := loaded
    else
      pure ()
    return allowed

  function liquidationLockedValue (id : Bytes32, user : Address) :
      Uint256 := do
    let locked ← ecmCall (fun resultVar => liquidationLockGetModule resultVar)
      [id, addressToWord user]
    return locked

  function liquidationLockExchange (id : Bytes32, user : Address, value : Uint256) :
      Uint256 := do
    let previous ← ecmCall (fun resultVar => liquidationLockExchangeModule resultVar)
      [id, addressToWord user, value]
    return previous

  function liquidationLocked (id : Bytes32, user : Address) : Bool := do
    let locked ← liquidationLockedValue id user
    return locked != ZERO

  function collateralAmount (id : Bytes32, user : Address, index : Uint256) : Uint256 := do
    let mut value := ZERO
    if index == 0 then
      let loaded ← structMember2 "positionSlot" id user "collateral[0]"
      value := loaded
    else
      pure ()
    if index == 1 then
      let loaded ← structMember2 "positionSlot" id user "collateral[1]"
      value := loaded
    else
      pure ()
    if index == 2 then
      let loaded ← structMember2 "positionSlot" id user "collateral[2]"
      value := loaded
    else
      pure ()
    if index == 3 then
      let loaded ← structMember2 "positionSlot" id user "collateral[3]"
      value := loaded
    else
      pure ()
    if index == 4 then
      let loaded ← structMember2 "positionSlot" id user "collateral[4]"
      value := loaded
    else
      pure ()
    if index == 5 then
      let loaded ← structMember2 "positionSlot" id user "collateral[5]"
      value := loaded
    else
      pure ()
    if index == 6 then
      let loaded ← structMember2 "positionSlot" id user "collateral[6]"
      value := loaded
    else
      pure ()
    if index == 7 then
      let loaded ← structMember2 "positionSlot" id user "collateral[7]"
      value := loaded
    else
      pure ()
    if index == 8 then
      let loaded ← structMember2 "positionSlot" id user "collateral[8]"
      value := loaded
    else
      pure ()
    if index == 9 then
      let loaded ← structMember2 "positionSlot" id user "collateral[9]"
      value := loaded
    else
      pure ()
    if index == 10 then
      let loaded ← structMember2 "positionSlot" id user "collateral[10]"
      value := loaded
    else
      pure ()
    if index == 11 then
      let loaded ← structMember2 "positionSlot" id user "collateral[11]"
      value := loaded
    else
      pure ()
    if index == 12 then
      let loaded ← structMember2 "positionSlot" id user "collateral[12]"
      value := loaded
    else
      pure ()
    if index == 13 then
      let loaded ← structMember2 "positionSlot" id user "collateral[13]"
      value := loaded
    else
      pure ()
    if index == 14 then
      let loaded ← structMember2 "positionSlot" id user "collateral[14]"
      value := loaded
    else
      pure ()
    if index == 15 then
      let loaded ← structMember2 "positionSlot" id user "collateral[15]"
      value := loaded
    else
      pure ()
    if index == 16 then
      let loaded ← structMember2 "positionSlot" id user "collateral[16]"
      value := loaded
    else
      pure ()
    if index == 17 then
      let loaded ← structMember2 "positionSlot" id user "collateral[17]"
      value := loaded
    else
      pure ()
    if index == 18 then
      let loaded ← structMember2 "positionSlot" id user "collateral[18]"
      value := loaded
    else
      pure ()
    if index == 19 then
      let loaded ← structMember2 "positionSlot" id user "collateral[19]"
      value := loaded
    else
      pure ()
    if index == 20 then
      let loaded ← structMember2 "positionSlot" id user "collateral[20]"
      value := loaded
    else
      pure ()
    if index == 21 then
      let loaded ← structMember2 "positionSlot" id user "collateral[21]"
      value := loaded
    else
      pure ()
    if index == 22 then
      let loaded ← structMember2 "positionSlot" id user "collateral[22]"
      value := loaded
    else
      pure ()
    if index == 23 then
      let loaded ← structMember2 "positionSlot" id user "collateral[23]"
      value := loaded
    else
      pure ()
    if index == 24 then
      let loaded ← structMember2 "positionSlot" id user "collateral[24]"
      value := loaded
    else
      pure ()
    if index == 25 then
      let loaded ← structMember2 "positionSlot" id user "collateral[25]"
      value := loaded
    else
      pure ()
    if index == 26 then
      let loaded ← structMember2 "positionSlot" id user "collateral[26]"
      value := loaded
    else
      pure ()
    if index == 27 then
      let loaded ← structMember2 "positionSlot" id user "collateral[27]"
      value := loaded
    else
      pure ()
    if index == 28 then
      let loaded ← structMember2 "positionSlot" id user "collateral[28]"
      value := loaded
    else
      pure ()
    if index == 29 then
      let loaded ← structMember2 "positionSlot" id user "collateral[29]"
      value := loaded
    else
      pure ()
    if index == 30 then
      let loaded ← structMember2 "positionSlot" id user "collateral[30]"
      value := loaded
    else
      pure ()
    if index == 31 then
      let loaded ← structMember2 "positionSlot" id user "collateral[31]"
      value := loaded
    else
      pure ()
    if index == 32 then
      let loaded ← structMember2 "positionSlot" id user "collateral[32]"
      value := loaded
    else
      pure ()
    if index == 33 then
      let loaded ← structMember2 "positionSlot" id user "collateral[33]"
      value := loaded
    else
      pure ()
    if index == 34 then
      let loaded ← structMember2 "positionSlot" id user "collateral[34]"
      value := loaded
    else
      pure ()
    if index == 35 then
      let loaded ← structMember2 "positionSlot" id user "collateral[35]"
      value := loaded
    else
      pure ()
    if index == 36 then
      let loaded ← structMember2 "positionSlot" id user "collateral[36]"
      value := loaded
    else
      pure ()
    if index == 37 then
      let loaded ← structMember2 "positionSlot" id user "collateral[37]"
      value := loaded
    else
      pure ()
    if index == 38 then
      let loaded ← structMember2 "positionSlot" id user "collateral[38]"
      value := loaded
    else
      pure ()
    if index == 39 then
      let loaded ← structMember2 "positionSlot" id user "collateral[39]"
      value := loaded
    else
      pure ()
    if index == 40 then
      let loaded ← structMember2 "positionSlot" id user "collateral[40]"
      value := loaded
    else
      pure ()
    if index == 41 then
      let loaded ← structMember2 "positionSlot" id user "collateral[41]"
      value := loaded
    else
      pure ()
    if index == 42 then
      let loaded ← structMember2 "positionSlot" id user "collateral[42]"
      value := loaded
    else
      pure ()
    if index == 43 then
      let loaded ← structMember2 "positionSlot" id user "collateral[43]"
      value := loaded
    else
      pure ()
    if index == 44 then
      let loaded ← structMember2 "positionSlot" id user "collateral[44]"
      value := loaded
    else
      pure ()
    if index == 45 then
      let loaded ← structMember2 "positionSlot" id user "collateral[45]"
      value := loaded
    else
      pure ()
    if index == 46 then
      let loaded ← structMember2 "positionSlot" id user "collateral[46]"
      value := loaded
    else
      pure ()
    if index == 47 then
      let loaded ← structMember2 "positionSlot" id user "collateral[47]"
      value := loaded
    else
      pure ()
    if index == 48 then
      let loaded ← structMember2 "positionSlot" id user "collateral[48]"
      value := loaded
    else
      pure ()
    if index == 49 then
      let loaded ← structMember2 "positionSlot" id user "collateral[49]"
      value := loaded
    else
      pure ()
    if index == 50 then
      let loaded ← structMember2 "positionSlot" id user "collateral[50]"
      value := loaded
    else
      pure ()
    if index == 51 then
      let loaded ← structMember2 "positionSlot" id user "collateral[51]"
      value := loaded
    else
      pure ()
    if index == 52 then
      let loaded ← structMember2 "positionSlot" id user "collateral[52]"
      value := loaded
    else
      pure ()
    if index == 53 then
      let loaded ← structMember2 "positionSlot" id user "collateral[53]"
      value := loaded
    else
      pure ()
    if index == 54 then
      let loaded ← structMember2 "positionSlot" id user "collateral[54]"
      value := loaded
    else
      pure ()
    if index == 55 then
      let loaded ← structMember2 "positionSlot" id user "collateral[55]"
      value := loaded
    else
      pure ()
    if index == 56 then
      let loaded ← structMember2 "positionSlot" id user "collateral[56]"
      value := loaded
    else
      pure ()
    if index == 57 then
      let loaded ← structMember2 "positionSlot" id user "collateral[57]"
      value := loaded
    else
      pure ()
    if index == 58 then
      let loaded ← structMember2 "positionSlot" id user "collateral[58]"
      value := loaded
    else
      pure ()
    if index == 59 then
      let loaded ← structMember2 "positionSlot" id user "collateral[59]"
      value := loaded
    else
      pure ()
    if index == 60 then
      let loaded ← structMember2 "positionSlot" id user "collateral[60]"
      value := loaded
    else
      pure ()
    if index == 61 then
      let loaded ← structMember2 "positionSlot" id user "collateral[61]"
      value := loaded
    else
      pure ()
    if index == 62 then
      let loaded ← structMember2 "positionSlot" id user "collateral[62]"
      value := loaded
    else
      pure ()
    if index == 63 then
      let loaded ← structMember2 "positionSlot" id user "collateral[63]"
      value := loaded
    else
      pure ()
    if index == 64 then
      let loaded ← structMember2 "positionSlot" id user "collateral[64]"
      value := loaded
    else
      pure ()
    if index == 65 then
      let loaded ← structMember2 "positionSlot" id user "collateral[65]"
      value := loaded
    else
      pure ()
    if index == 66 then
      let loaded ← structMember2 "positionSlot" id user "collateral[66]"
      value := loaded
    else
      pure ()
    if index == 67 then
      let loaded ← structMember2 "positionSlot" id user "collateral[67]"
      value := loaded
    else
      pure ()
    if index == 68 then
      let loaded ← structMember2 "positionSlot" id user "collateral[68]"
      value := loaded
    else
      pure ()
    if index == 69 then
      let loaded ← structMember2 "positionSlot" id user "collateral[69]"
      value := loaded
    else
      pure ()
    if index == 70 then
      let loaded ← structMember2 "positionSlot" id user "collateral[70]"
      value := loaded
    else
      pure ()
    if index == 71 then
      let loaded ← structMember2 "positionSlot" id user "collateral[71]"
      value := loaded
    else
      pure ()
    if index == 72 then
      let loaded ← structMember2 "positionSlot" id user "collateral[72]"
      value := loaded
    else
      pure ()
    if index == 73 then
      let loaded ← structMember2 "positionSlot" id user "collateral[73]"
      value := loaded
    else
      pure ()
    if index == 74 then
      let loaded ← structMember2 "positionSlot" id user "collateral[74]"
      value := loaded
    else
      pure ()
    if index == 75 then
      let loaded ← structMember2 "positionSlot" id user "collateral[75]"
      value := loaded
    else
      pure ()
    if index == 76 then
      let loaded ← structMember2 "positionSlot" id user "collateral[76]"
      value := loaded
    else
      pure ()
    if index == 77 then
      let loaded ← structMember2 "positionSlot" id user "collateral[77]"
      value := loaded
    else
      pure ()
    if index == 78 then
      let loaded ← structMember2 "positionSlot" id user "collateral[78]"
      value := loaded
    else
      pure ()
    if index == 79 then
      let loaded ← structMember2 "positionSlot" id user "collateral[79]"
      value := loaded
    else
      pure ()
    if index == 80 then
      let loaded ← structMember2 "positionSlot" id user "collateral[80]"
      value := loaded
    else
      pure ()
    if index == 81 then
      let loaded ← structMember2 "positionSlot" id user "collateral[81]"
      value := loaded
    else
      pure ()
    if index == 82 then
      let loaded ← structMember2 "positionSlot" id user "collateral[82]"
      value := loaded
    else
      pure ()
    if index == 83 then
      let loaded ← structMember2 "positionSlot" id user "collateral[83]"
      value := loaded
    else
      pure ()
    if index == 84 then
      let loaded ← structMember2 "positionSlot" id user "collateral[84]"
      value := loaded
    else
      pure ()
    if index == 85 then
      let loaded ← structMember2 "positionSlot" id user "collateral[85]"
      value := loaded
    else
      pure ()
    if index == 86 then
      let loaded ← structMember2 "positionSlot" id user "collateral[86]"
      value := loaded
    else
      pure ()
    if index == 87 then
      let loaded ← structMember2 "positionSlot" id user "collateral[87]"
      value := loaded
    else
      pure ()
    if index == 88 then
      let loaded ← structMember2 "positionSlot" id user "collateral[88]"
      value := loaded
    else
      pure ()
    if index == 89 then
      let loaded ← structMember2 "positionSlot" id user "collateral[89]"
      value := loaded
    else
      pure ()
    if index == 90 then
      let loaded ← structMember2 "positionSlot" id user "collateral[90]"
      value := loaded
    else
      pure ()
    if index == 91 then
      let loaded ← structMember2 "positionSlot" id user "collateral[91]"
      value := loaded
    else
      pure ()
    if index == 92 then
      let loaded ← structMember2 "positionSlot" id user "collateral[92]"
      value := loaded
    else
      pure ()
    if index == 93 then
      let loaded ← structMember2 "positionSlot" id user "collateral[93]"
      value := loaded
    else
      pure ()
    if index == 94 then
      let loaded ← structMember2 "positionSlot" id user "collateral[94]"
      value := loaded
    else
      pure ()
    if index == 95 then
      let loaded ← structMember2 "positionSlot" id user "collateral[95]"
      value := loaded
    else
      pure ()
    if index == 96 then
      let loaded ← structMember2 "positionSlot" id user "collateral[96]"
      value := loaded
    else
      pure ()
    if index == 97 then
      let loaded ← structMember2 "positionSlot" id user "collateral[97]"
      value := loaded
    else
      pure ()
    if index == 98 then
      let loaded ← structMember2 "positionSlot" id user "collateral[98]"
      value := loaded
    else
      pure ()
    if index == 99 then
      let loaded ← structMember2 "positionSlot" id user "collateral[99]"
      value := loaded
    else
      pure ()
    if index == 100 then
      let loaded ← structMember2 "positionSlot" id user "collateral[100]"
      value := loaded
    else
      pure ()
    if index == 101 then
      let loaded ← structMember2 "positionSlot" id user "collateral[101]"
      value := loaded
    else
      pure ()
    if index == 102 then
      let loaded ← structMember2 "positionSlot" id user "collateral[102]"
      value := loaded
    else
      pure ()
    if index == 103 then
      let loaded ← structMember2 "positionSlot" id user "collateral[103]"
      value := loaded
    else
      pure ()
    if index == 104 then
      let loaded ← structMember2 "positionSlot" id user "collateral[104]"
      value := loaded
    else
      pure ()
    if index == 105 then
      let loaded ← structMember2 "positionSlot" id user "collateral[105]"
      value := loaded
    else
      pure ()
    if index == 106 then
      let loaded ← structMember2 "positionSlot" id user "collateral[106]"
      value := loaded
    else
      pure ()
    if index == 107 then
      let loaded ← structMember2 "positionSlot" id user "collateral[107]"
      value := loaded
    else
      pure ()
    if index == 108 then
      let loaded ← structMember2 "positionSlot" id user "collateral[108]"
      value := loaded
    else
      pure ()
    if index == 109 then
      let loaded ← structMember2 "positionSlot" id user "collateral[109]"
      value := loaded
    else
      pure ()
    if index == 110 then
      let loaded ← structMember2 "positionSlot" id user "collateral[110]"
      value := loaded
    else
      pure ()
    if index == 111 then
      let loaded ← structMember2 "positionSlot" id user "collateral[111]"
      value := loaded
    else
      pure ()
    if index == 112 then
      let loaded ← structMember2 "positionSlot" id user "collateral[112]"
      value := loaded
    else
      pure ()
    if index == 113 then
      let loaded ← structMember2 "positionSlot" id user "collateral[113]"
      value := loaded
    else
      pure ()
    if index == 114 then
      let loaded ← structMember2 "positionSlot" id user "collateral[114]"
      value := loaded
    else
      pure ()
    if index == 115 then
      let loaded ← structMember2 "positionSlot" id user "collateral[115]"
      value := loaded
    else
      pure ()
    if index == 116 then
      let loaded ← structMember2 "positionSlot" id user "collateral[116]"
      value := loaded
    else
      pure ()
    if index == 117 then
      let loaded ← structMember2 "positionSlot" id user "collateral[117]"
      value := loaded
    else
      pure ()
    if index == 118 then
      let loaded ← structMember2 "positionSlot" id user "collateral[118]"
      value := loaded
    else
      pure ()
    if index == 119 then
      let loaded ← structMember2 "positionSlot" id user "collateral[119]"
      value := loaded
    else
      pure ()
    if index == 120 then
      let loaded ← structMember2 "positionSlot" id user "collateral[120]"
      value := loaded
    else
      pure ()
    if index == 121 then
      let loaded ← structMember2 "positionSlot" id user "collateral[121]"
      value := loaded
    else
      pure ()
    if index == 122 then
      let loaded ← structMember2 "positionSlot" id user "collateral[122]"
      value := loaded
    else
      pure ()
    if index == 123 then
      let loaded ← structMember2 "positionSlot" id user "collateral[123]"
      value := loaded
    else
      pure ()
    if index == 124 then
      let loaded ← structMember2 "positionSlot" id user "collateral[124]"
      value := loaded
    else
      pure ()
    if index == 125 then
      let loaded ← structMember2 "positionSlot" id user "collateral[125]"
      value := loaded
    else
      pure ()
    if index == 126 then
      let loaded ← structMember2 "positionSlot" id user "collateral[126]"
      value := loaded
    else
      pure ()
    if index == 127 then
      let loaded ← structMember2 "positionSlot" id user "collateral[127]"
      value := loaded
    else
      pure ()
    return value

  function liquidationDebtSnapshot
      (id : Bytes32, borrower : Address, collateralCount : Uint256,
        collateralBitmapValue : Uint256,
        collateralParamsOffset : Uint256, originalDebt : Uint256)
      local_obligations [liquidation_debt_snapshot_calldata_collateral_params := assumed
        "Temporary Midnight source-faithfulness bridge reads CollateralParams fields from calldata because direct nested dynamic array element projection is not yet supported in this liquidate path."] :
      Tuple [Uint256, Uint256] := do
    let mut maxDebtValue := ZERO
    let mut badDebt := originalDebt
    forEach "i" collateralCount (do
      let mask := shl i ONE
      if bitAnd collateralBitmapValue mask > ZERO then
        let activeCollateral ← collateralAmount id borrower i
        let collateralParamOffset := add (add collateralParamsOffset 32)
          (mul i 128)
        let oracle := wordToAddress (calldataload (add collateralParamOffset 96))
        let price ← oraclePrice oracle
        let lltv := calldataload (add collateralParamOffset 32)
        let maxLifValue := calldataload (add collateralParamOffset 64)
        let collateralDebtValue :=
          mulDivDown (mulDivDown activeCollateral price ORACLE_PRICE_SCALE) lltv WAD
        maxDebtValue := add maxDebtValue collateralDebtValue
        let repayable :=
          mulDivUp (mulDivUp activeCollateral price ORACLE_PRICE_SCALE) WAD maxLifValue
        if badDebt > repayable then
          badDebt := sub badDebt repayable
        else
          badDebt := ZERO
      else
        pure ())
    return (maxDebtValue, badDebt)

  function writeCollateralAmount
      (id : Bytes32, user : Address, index : Uint256, value : Uint256) :
      Uint256 := do
    if index == 0 then
      setStructMember2 "positionSlot" id user "collateral[0]" value
    else
      pure ()
    if index == 1 then
      setStructMember2 "positionSlot" id user "collateral[1]" value
    else
      pure ()
    if index == 2 then
      setStructMember2 "positionSlot" id user "collateral[2]" value
    else
      pure ()
    if index == 3 then
      setStructMember2 "positionSlot" id user "collateral[3]" value
    else
      pure ()
    if index == 4 then
      setStructMember2 "positionSlot" id user "collateral[4]" value
    else
      pure ()
    if index == 5 then
      setStructMember2 "positionSlot" id user "collateral[5]" value
    else
      pure ()
    if index == 6 then
      setStructMember2 "positionSlot" id user "collateral[6]" value
    else
      pure ()
    if index == 7 then
      setStructMember2 "positionSlot" id user "collateral[7]" value
    else
      pure ()
    if index == 8 then
      setStructMember2 "positionSlot" id user "collateral[8]" value
    else
      pure ()
    if index == 9 then
      setStructMember2 "positionSlot" id user "collateral[9]" value
    else
      pure ()
    if index == 10 then
      setStructMember2 "positionSlot" id user "collateral[10]" value
    else
      pure ()
    if index == 11 then
      setStructMember2 "positionSlot" id user "collateral[11]" value
    else
      pure ()
    if index == 12 then
      setStructMember2 "positionSlot" id user "collateral[12]" value
    else
      pure ()
    if index == 13 then
      setStructMember2 "positionSlot" id user "collateral[13]" value
    else
      pure ()
    if index == 14 then
      setStructMember2 "positionSlot" id user "collateral[14]" value
    else
      pure ()
    if index == 15 then
      setStructMember2 "positionSlot" id user "collateral[15]" value
    else
      pure ()
    if index == 16 then
      setStructMember2 "positionSlot" id user "collateral[16]" value
    else
      pure ()
    if index == 17 then
      setStructMember2 "positionSlot" id user "collateral[17]" value
    else
      pure ()
    if index == 18 then
      setStructMember2 "positionSlot" id user "collateral[18]" value
    else
      pure ()
    if index == 19 then
      setStructMember2 "positionSlot" id user "collateral[19]" value
    else
      pure ()
    if index == 20 then
      setStructMember2 "positionSlot" id user "collateral[20]" value
    else
      pure ()
    if index == 21 then
      setStructMember2 "positionSlot" id user "collateral[21]" value
    else
      pure ()
    if index == 22 then
      setStructMember2 "positionSlot" id user "collateral[22]" value
    else
      pure ()
    if index == 23 then
      setStructMember2 "positionSlot" id user "collateral[23]" value
    else
      pure ()
    if index == 24 then
      setStructMember2 "positionSlot" id user "collateral[24]" value
    else
      pure ()
    if index == 25 then
      setStructMember2 "positionSlot" id user "collateral[25]" value
    else
      pure ()
    if index == 26 then
      setStructMember2 "positionSlot" id user "collateral[26]" value
    else
      pure ()
    if index == 27 then
      setStructMember2 "positionSlot" id user "collateral[27]" value
    else
      pure ()
    if index == 28 then
      setStructMember2 "positionSlot" id user "collateral[28]" value
    else
      pure ()
    if index == 29 then
      setStructMember2 "positionSlot" id user "collateral[29]" value
    else
      pure ()
    if index == 30 then
      setStructMember2 "positionSlot" id user "collateral[30]" value
    else
      pure ()
    if index == 31 then
      setStructMember2 "positionSlot" id user "collateral[31]" value
    else
      pure ()
    if index == 32 then
      setStructMember2 "positionSlot" id user "collateral[32]" value
    else
      pure ()
    if index == 33 then
      setStructMember2 "positionSlot" id user "collateral[33]" value
    else
      pure ()
    if index == 34 then
      setStructMember2 "positionSlot" id user "collateral[34]" value
    else
      pure ()
    if index == 35 then
      setStructMember2 "positionSlot" id user "collateral[35]" value
    else
      pure ()
    if index == 36 then
      setStructMember2 "positionSlot" id user "collateral[36]" value
    else
      pure ()
    if index == 37 then
      setStructMember2 "positionSlot" id user "collateral[37]" value
    else
      pure ()
    if index == 38 then
      setStructMember2 "positionSlot" id user "collateral[38]" value
    else
      pure ()
    if index == 39 then
      setStructMember2 "positionSlot" id user "collateral[39]" value
    else
      pure ()
    if index == 40 then
      setStructMember2 "positionSlot" id user "collateral[40]" value
    else
      pure ()
    if index == 41 then
      setStructMember2 "positionSlot" id user "collateral[41]" value
    else
      pure ()
    if index == 42 then
      setStructMember2 "positionSlot" id user "collateral[42]" value
    else
      pure ()
    if index == 43 then
      setStructMember2 "positionSlot" id user "collateral[43]" value
    else
      pure ()
    if index == 44 then
      setStructMember2 "positionSlot" id user "collateral[44]" value
    else
      pure ()
    if index == 45 then
      setStructMember2 "positionSlot" id user "collateral[45]" value
    else
      pure ()
    if index == 46 then
      setStructMember2 "positionSlot" id user "collateral[46]" value
    else
      pure ()
    if index == 47 then
      setStructMember2 "positionSlot" id user "collateral[47]" value
    else
      pure ()
    if index == 48 then
      setStructMember2 "positionSlot" id user "collateral[48]" value
    else
      pure ()
    if index == 49 then
      setStructMember2 "positionSlot" id user "collateral[49]" value
    else
      pure ()
    if index == 50 then
      setStructMember2 "positionSlot" id user "collateral[50]" value
    else
      pure ()
    if index == 51 then
      setStructMember2 "positionSlot" id user "collateral[51]" value
    else
      pure ()
    if index == 52 then
      setStructMember2 "positionSlot" id user "collateral[52]" value
    else
      pure ()
    if index == 53 then
      setStructMember2 "positionSlot" id user "collateral[53]" value
    else
      pure ()
    if index == 54 then
      setStructMember2 "positionSlot" id user "collateral[54]" value
    else
      pure ()
    if index == 55 then
      setStructMember2 "positionSlot" id user "collateral[55]" value
    else
      pure ()
    if index == 56 then
      setStructMember2 "positionSlot" id user "collateral[56]" value
    else
      pure ()
    if index == 57 then
      setStructMember2 "positionSlot" id user "collateral[57]" value
    else
      pure ()
    if index == 58 then
      setStructMember2 "positionSlot" id user "collateral[58]" value
    else
      pure ()
    if index == 59 then
      setStructMember2 "positionSlot" id user "collateral[59]" value
    else
      pure ()
    if index == 60 then
      setStructMember2 "positionSlot" id user "collateral[60]" value
    else
      pure ()
    if index == 61 then
      setStructMember2 "positionSlot" id user "collateral[61]" value
    else
      pure ()
    if index == 62 then
      setStructMember2 "positionSlot" id user "collateral[62]" value
    else
      pure ()
    if index == 63 then
      setStructMember2 "positionSlot" id user "collateral[63]" value
    else
      pure ()
    if index == 64 then
      setStructMember2 "positionSlot" id user "collateral[64]" value
    else
      pure ()
    if index == 65 then
      setStructMember2 "positionSlot" id user "collateral[65]" value
    else
      pure ()
    if index == 66 then
      setStructMember2 "positionSlot" id user "collateral[66]" value
    else
      pure ()
    if index == 67 then
      setStructMember2 "positionSlot" id user "collateral[67]" value
    else
      pure ()
    if index == 68 then
      setStructMember2 "positionSlot" id user "collateral[68]" value
    else
      pure ()
    if index == 69 then
      setStructMember2 "positionSlot" id user "collateral[69]" value
    else
      pure ()
    if index == 70 then
      setStructMember2 "positionSlot" id user "collateral[70]" value
    else
      pure ()
    if index == 71 then
      setStructMember2 "positionSlot" id user "collateral[71]" value
    else
      pure ()
    if index == 72 then
      setStructMember2 "positionSlot" id user "collateral[72]" value
    else
      pure ()
    if index == 73 then
      setStructMember2 "positionSlot" id user "collateral[73]" value
    else
      pure ()
    if index == 74 then
      setStructMember2 "positionSlot" id user "collateral[74]" value
    else
      pure ()
    if index == 75 then
      setStructMember2 "positionSlot" id user "collateral[75]" value
    else
      pure ()
    if index == 76 then
      setStructMember2 "positionSlot" id user "collateral[76]" value
    else
      pure ()
    if index == 77 then
      setStructMember2 "positionSlot" id user "collateral[77]" value
    else
      pure ()
    if index == 78 then
      setStructMember2 "positionSlot" id user "collateral[78]" value
    else
      pure ()
    if index == 79 then
      setStructMember2 "positionSlot" id user "collateral[79]" value
    else
      pure ()
    if index == 80 then
      setStructMember2 "positionSlot" id user "collateral[80]" value
    else
      pure ()
    if index == 81 then
      setStructMember2 "positionSlot" id user "collateral[81]" value
    else
      pure ()
    if index == 82 then
      setStructMember2 "positionSlot" id user "collateral[82]" value
    else
      pure ()
    if index == 83 then
      setStructMember2 "positionSlot" id user "collateral[83]" value
    else
      pure ()
    if index == 84 then
      setStructMember2 "positionSlot" id user "collateral[84]" value
    else
      pure ()
    if index == 85 then
      setStructMember2 "positionSlot" id user "collateral[85]" value
    else
      pure ()
    if index == 86 then
      setStructMember2 "positionSlot" id user "collateral[86]" value
    else
      pure ()
    if index == 87 then
      setStructMember2 "positionSlot" id user "collateral[87]" value
    else
      pure ()
    if index == 88 then
      setStructMember2 "positionSlot" id user "collateral[88]" value
    else
      pure ()
    if index == 89 then
      setStructMember2 "positionSlot" id user "collateral[89]" value
    else
      pure ()
    if index == 90 then
      setStructMember2 "positionSlot" id user "collateral[90]" value
    else
      pure ()
    if index == 91 then
      setStructMember2 "positionSlot" id user "collateral[91]" value
    else
      pure ()
    if index == 92 then
      setStructMember2 "positionSlot" id user "collateral[92]" value
    else
      pure ()
    if index == 93 then
      setStructMember2 "positionSlot" id user "collateral[93]" value
    else
      pure ()
    if index == 94 then
      setStructMember2 "positionSlot" id user "collateral[94]" value
    else
      pure ()
    if index == 95 then
      setStructMember2 "positionSlot" id user "collateral[95]" value
    else
      pure ()
    if index == 96 then
      setStructMember2 "positionSlot" id user "collateral[96]" value
    else
      pure ()
    if index == 97 then
      setStructMember2 "positionSlot" id user "collateral[97]" value
    else
      pure ()
    if index == 98 then
      setStructMember2 "positionSlot" id user "collateral[98]" value
    else
      pure ()
    if index == 99 then
      setStructMember2 "positionSlot" id user "collateral[99]" value
    else
      pure ()
    if index == 100 then
      setStructMember2 "positionSlot" id user "collateral[100]" value
    else
      pure ()
    if index == 101 then
      setStructMember2 "positionSlot" id user "collateral[101]" value
    else
      pure ()
    if index == 102 then
      setStructMember2 "positionSlot" id user "collateral[102]" value
    else
      pure ()
    if index == 103 then
      setStructMember2 "positionSlot" id user "collateral[103]" value
    else
      pure ()
    if index == 104 then
      setStructMember2 "positionSlot" id user "collateral[104]" value
    else
      pure ()
    if index == 105 then
      setStructMember2 "positionSlot" id user "collateral[105]" value
    else
      pure ()
    if index == 106 then
      setStructMember2 "positionSlot" id user "collateral[106]" value
    else
      pure ()
    if index == 107 then
      setStructMember2 "positionSlot" id user "collateral[107]" value
    else
      pure ()
    if index == 108 then
      setStructMember2 "positionSlot" id user "collateral[108]" value
    else
      pure ()
    if index == 109 then
      setStructMember2 "positionSlot" id user "collateral[109]" value
    else
      pure ()
    if index == 110 then
      setStructMember2 "positionSlot" id user "collateral[110]" value
    else
      pure ()
    if index == 111 then
      setStructMember2 "positionSlot" id user "collateral[111]" value
    else
      pure ()
    if index == 112 then
      setStructMember2 "positionSlot" id user "collateral[112]" value
    else
      pure ()
    if index == 113 then
      setStructMember2 "positionSlot" id user "collateral[113]" value
    else
      pure ()
    if index == 114 then
      setStructMember2 "positionSlot" id user "collateral[114]" value
    else
      pure ()
    if index == 115 then
      setStructMember2 "positionSlot" id user "collateral[115]" value
    else
      pure ()
    if index == 116 then
      setStructMember2 "positionSlot" id user "collateral[116]" value
    else
      pure ()
    if index == 117 then
      setStructMember2 "positionSlot" id user "collateral[117]" value
    else
      pure ()
    if index == 118 then
      setStructMember2 "positionSlot" id user "collateral[118]" value
    else
      pure ()
    if index == 119 then
      setStructMember2 "positionSlot" id user "collateral[119]" value
    else
      pure ()
    if index == 120 then
      setStructMember2 "positionSlot" id user "collateral[120]" value
    else
      pure ()
    if index == 121 then
      setStructMember2 "positionSlot" id user "collateral[121]" value
    else
      pure ()
    if index == 122 then
      setStructMember2 "positionSlot" id user "collateral[122]" value
    else
      pure ()
    if index == 123 then
      setStructMember2 "positionSlot" id user "collateral[123]" value
    else
      pure ()
    if index == 124 then
      setStructMember2 "positionSlot" id user "collateral[124]" value
    else
      pure ()
    if index == 125 then
      setStructMember2 "positionSlot" id user "collateral[125]" value
    else
      pure ()
    if index == 126 then
      setStructMember2 "positionSlot" id user "collateral[126]" value
    else
      pure ()
    if index == 127 then
      setStructMember2 "positionSlot" id user "collateral[127]" value
    else
      pure ()
    return ZERO

  function allow_post_interaction_writes liquidate
      (market : Market, collateralIndex : Uint256, seizedAssets : Uint256,
        repaidUnits : Uint256, borrower : Address, postMaturityMode : Bool,
        receiver : Address, callback : Address, data : Bytes)
      local_obligations [liquidate_calldata_collateral_params := assumed
        "Temporary Midnight source-faithfulness bridge reads CollateralParams fields from calldata because direct nested dynamic array element projection is not yet supported in this liquidate path."] :
      Tuple [Uint256, Uint256] := do
    let sender ← msgSender
    let id ← toId market
    let debtLoaded ← structMember2 "positionSlot" id borrower "debt"
    let mut debt := debtLoaded
    let totalUnitsValue ← structMember "marketStateSlot" id "totalUnits"
    requireError (seizedAssets == ZERO || repaidUnits == ZERO) InconsistentInput()
    requireError (debt > ZERO) NotBorrower()
    let canLiquidate ←
      liquidatorGateCanLiquidateOrDefault market.liquidatorGate sender
    requireError (market.liquidatorGate == 0 || canLiquidate != ZERO)
      LiquidatorGatedFromLiquidating()
    let collateralCount := arrayLength market.collateralParams
    if collateralIndex < collateralCount then
      pure ()
    else
      ecmDo solidityPanicModule [0x32]
    let _collateralIndexBoundsCheck ←
      collateralMaxLifAt market.collateralParams collateralIndex
    let marketDataOffset := add (calldataload 4) 4
    let collateralParamsOffset := add marketDataOffset
      (calldataload (add marketDataOffset 32))
    let collateralBitmapValue ← structMember2 "positionSlot" id borrower "collateralBitmap"
    let collateralMask := shl collateralIndex ONE
    if seizedAssets > ZERO || repaidUnits > ZERO then
      require (bitAnd collateralBitmapValue collateralMask > ZERO) "inactive collateral"
    else
      pure ()
    let originalDebt := debt
    let (maxDebtValue, badDebt) ←
      liquidationDebtSnapshot id borrower collateralCount collateralBitmapValue
        collateralParamsOffset originalDebt
    let selectedCollateralParamOffsetForPrice := add (add collateralParamsOffset 32)
      (mul collateralIndex 128)
    let selectedOracle :=
      wordToAddress (calldataload (add selectedCollateralParamOffsetForPrice 96))
    let liquidatedCollatPrice ← oraclePrice selectedOracle
    let now ← blockTimestamp
    let lockedBeforeLiquidation ← liquidationLockedValue id borrower
    requireError (lockedBeforeLiquidation == ZERO) NotLiquidatable()
    if postMaturityMode then
      requireError (now > market.maturity) NotLiquidatable()
    else
      requireError (originalDebt > maxDebtValue) NotLiquidatable()
    if badDebt > ZERO then
      setStructMember2 "positionSlot" id borrower "debt" (sub debt badDebt)
      debt := sub debt badDebt
      let oldLossFactor ← structMember "marketStateSlot" id "lossFactor"
      let newLossFactor :=
        sub MAX_LOSS_FACTOR
          (mulDivDown (sub MAX_LOSS_FACTOR oldLossFactor)
            (sub totalUnitsValue badDebt)
            totalUnitsValue)
      setStructMember "marketStateSlot" id "lossFactor" newLossFactor
      setStructMember "marketStateSlot" id "totalUnits"
        (sub totalUnitsValue badDebt)
      let oldContinuousFeeCredit ←
        structMember "marketStateSlot" id "continuousFeeCredit"
      let mut newContinuousFeeCredit := ZERO
      if oldLossFactor < MAX_LOSS_FACTOR then
        newContinuousFeeCredit :=
          mulDivDown oldContinuousFeeCredit
            (sub MAX_LOSS_FACTOR newLossFactor)
            (sub MAX_LOSS_FACTOR oldLossFactor)
      else
        pure ()
      setStructMember "marketStateSlot" id "continuousFeeCredit"
        newContinuousFeeCredit
    else
      pure ()
    let mut outSeizedAssets := seizedAssets
    let mut outRepaidUnits := repaidUnits
    if outRepaidUnits > ZERO || outSeizedAssets > ZERO then
      let selectedCollateralParamOffset := add (add collateralParamsOffset 32)
        (mul collateralIndex 128)
      let maxLifValue := calldataload (add selectedCollateralParamOffset 64)
      let mut lif := maxLifValue
      if postMaturityMode then
        let elapsed := sub now market.maturity
        let postMaturityLif := add WAD
          (div (mul (sub maxLifValue WAD) elapsed) TIME_TO_MAX_LIF)
        if maxLifValue <= postMaturityLif then
          lif := maxLifValue
        else
          lif := postMaturityLif
      else
        pure ()
      if outSeizedAssets > ZERO then
        outRepaidUnits :=
          mulDivUp (mulDivUp outSeizedAssets liquidatedCollatPrice ORACLE_PRICE_SCALE)
            WAD lif
      else
        outSeizedAssets :=
          mulDivDown (mulDivDown outRepaidUnits lif WAD)
            ORACLE_PRICE_SCALE liquidatedCollatPrice
      if postMaturityMode then
        pure ()
      else
        let lltv := calldataload (add selectedCollateralParamOffset 32)
        let mut maxRepaidValue :=
          115792089237316195423570985008687907853269984665640564039457584007913129639935
        if lltv < WAD then
          maxRepaidValue :=
            mulDivUp (sub debt maxDebtValue)
              (mul WAD WAD)
              (sub (mul WAD WAD) (mul lif lltv))
        else
          pure ()
        let oldCollateralForRcf ← collateralAmount id borrower collateralIndex
        let collateralRepayCapacity :=
          mulDivDown
            (mulDivDown oldCollateralForRcf liquidatedCollatPrice ORACLE_PRICE_SCALE)
            WAD lif
        let mut capacityShortfall := ZERO
        if collateralRepayCapacity > maxRepaidValue then
          capacityShortfall := sub collateralRepayCapacity maxRepaidValue
        else
          pure ()
        requireError (outRepaidUnits <= maxRepaidValue ||
          capacityShortfall < market.rcfThreshold)
          RecoveryCloseFactorConditionsViolated()
      let oldCollateral ← collateralAmount id borrower collateralIndex
      if oldCollateral < outSeizedAssets then
        ecmDo solidityPanicModule [0x11]
      else
        pure ()
      let newCollateral := sub oldCollateral outSeizedAssets
      let _writeOk ← writeCollateralAmount id borrower collateralIndex newCollateral
      if newCollateral == ZERO then
        if outSeizedAssets > ZERO then
          let oldBitmap ← structMember2 "positionSlot" id borrower "collateralBitmap"
          let mask := shl collateralIndex ONE
          let newBitmap := bitAnd oldBitmap (bitNot mask)
          setStructMember2 "positionSlot" id borrower "collateralBitmap" newBitmap
        else
          pure ()
      else
        pure ()
      let withdrawableAmount ← structMember "marketStateSlot" id "withdrawable"
      setStructMember "marketStateSlot" id "withdrawable"
        (add withdrawableAmount outRepaidUnits)
      if debt < outRepaidUnits then
        ecmDo solidityPanicModule [0x11]
      else
        pure ()
      let newDebtAfterRepay := sub debt outRepaidUnits
      setStructMember2 "positionSlot" id borrower "debt" newDebtAfterRepay
    else
      pure ()
    let selectedCollateralParamOffset := add (add collateralParamsOffset 32)
      (mul collateralIndex 128)
    let collateralToken := wordToAddress (calldataload selectedCollateralParamOffset)
    let mut payer := sender
    if callback != 0 then
      payer := callback
    else
      pure ()
    let latestLossFactorLoaded ← structMember "marketStateSlot" id "lossFactor"
    let latestContinuousFeeCreditLoaded ←
      structMember "marketStateSlot" id "continuousFeeCredit"
    emit "Liquidate"
      [sender, id, collateralToken, outSeizedAssets, outRepaidUnits, borrower,
        postMaturityMode, receiver, payer, badDebt,
        add latestLossFactorLoaded ZERO,
        add latestContinuousFeeCreditLoaded ZERO]
    safeTransfer collateralToken receiver outSeizedAssets
    if callback != 0 then
      ecmDo liquidateCallbackModule
        [addressToWord callback, addressToWord sender, id, collateralIndex,
          outSeizedAssets, outRepaidUnits, addressToWord borrower,
          addressToWord receiver, badDebt]
    else
      pure ()
    let self ← contractAddress
    safeTransferFrom market.loanToken payer self outRepaidUnits
    return (outSeizedAssets, outRepaidUnits)

  function isHealthy (market : Market, id : Bytes32, borrower : Address) : Bool := do
    let debt ← structMember2 "positionSlot" id borrower "debt"
    if debt == ZERO then
      return true
    else
      pure ()
    let collateralCount := arrayLength market.collateralParams
    let collateralBitmapValue ← structMember2 "positionSlot" id borrower "collateralBitmap"
    let mut maxDebt := ZERO
    forEach "i" collateralCount (do
      let mask := shl i ONE
      if bitAnd collateralBitmapValue mask > ZERO then
        let activeCollateral ← collateralAmount id borrower i
        let oracle ← collateralOracleAt market.collateralParams i
        let price ← oraclePrice oracle
        let lltv ← collateralLltvAt market.collateralParams i
        let collateralValue :=
          mulDivDown activeCollateral price ORACLE_PRICE_SCALE
        maxDebt := add maxDebt (mulDivDown collateralValue lltv WAD)
      else
        pure ())
    return debt <= maxDebt

  function allow_post_interaction_writes setCollateralAmount
      (id : Bytes32, user : Address, index : Uint256, value : Uint256) : Unit := do
    let _writeOk ← writeCollateralAmount id user index value

  function allow_post_interaction_writes supplyCollateral
      (market : Market, collateralIndex : Uint256, assets : Uint256,
        onBehalf : Address) : Unit := do
    let sender ← msgSender
    let authorized ← isAuthorized onBehalf sender
    requireError (onBehalf == sender || authorized) Unauthorized()
    let id ← toId market
    let oldCollateral ← collateralAmount id onBehalf collateralIndex
    let newCollateral := add oldCollateral assets
    requireError (newCollateral <= MAX_LOSS_FACTOR) CastOverflow()
    let oldBitmap ← structMember2 "positionSlot" id onBehalf "collateralBitmap"
    let mask := shl collateralIndex ONE
    if oldCollateral == ZERO then
      if assets > ZERO then
        let newBitmap := bitOr oldBitmap mask
        setStructMember2 "positionSlot" id onBehalf "collateralBitmap" newBitmap
        let activeCount ← countBits128 newBitmap
        requireError (activeCount <= MAX_COLLATERALS_PER_BORROWER) TooManyActivatedCollaterals()
      else
        pure ()
    else
      pure ()
    let collateralToken ← collateralTokenAt market.collateralParams collateralIndex
    let self ← contractAddress
    safeTransferFrom collateralToken sender self assets
    setCollateralAmount id onBehalf collateralIndex newCollateral

  function allow_post_interaction_writes withdrawCollateral
      (market : Market, collateralIndex : Uint256, assets : Uint256,
        onBehalf : Address, receiver : Address) : Unit := do
    let sender ← msgSender
    let authorized ← isAuthorized onBehalf sender
    requireError (onBehalf == sender || authorized) Unauthorized()
    let id ← toId market
    let oldCollateral ← collateralAmount id onBehalf collateralIndex
    let newCollateral := sub oldCollateral assets
    let debt ← structMember2 "positionSlot" id onBehalf "debt"
    if debt > ZERO then
      let lltv ← collateralLltvAt market.collateralParams collateralIndex
      let requiredCollateral := mulDivUp debt 1000000000000000000 lltv
      requireError (newCollateral >= requiredCollateral) UnhealthyBorrower()
    else
      pure ()
    if newCollateral == ZERO then
      if assets > ZERO then
        let oldBitmap ← structMember2 "positionSlot" id onBehalf "collateralBitmap"
        let mask := shl collateralIndex ONE
        let newBitmap := bitAnd oldBitmap (bitNot mask)
        setStructMember2 "positionSlot" id onBehalf "collateralBitmap" newBitmap
      else
        pure ()
    else
      pure ()
    let collateralToken ← collateralTokenAt market.collateralParams collateralIndex
    safeTransfer collateralToken receiver assets
    setCollateralAmount id onBehalf collateralIndex newCollateral

  function allow_post_interaction_writes flashLoan
      (tokens : Array Address, assets : Array Uint256, callback : Address,
        data : Bytes) : Unit := do
    let tokenCount := arrayLength tokens
    let assetCount := arrayLength assets
    requireError (tokenCount == assetCount) InconsistentInput()
    forEach "i" tokenCount (do
      safeTransfer (arrayElement tokens i) callback (arrayElement assets i))
    let sender ← msgSender
    ecmDo flashLoanCallbackModule [addressToWord callback, addressToWord sender]
    forEach "i" tokenCount (do
      let self ← contractAddress
      safeTransferFrom (arrayElement tokens i) callback self (arrayElement assets i))

  function collateral (id : Bytes32, user : Address, index : Uint256) : Uint256 := do
    let value ← collateralAmount id user index
    return value

  function pendingFee (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "pendingFee"
    return value

  function lastAccrual (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "lastAccrual"
    return value

  function lastLossFactor (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "lastLossFactor"
    return value

  function collateralBitmap (id : Bytes32, user : Address) : Uint256 := do
    let value ← structMember2 "positionSlot" id user "collateralBitmap"
    return value

end Midnight.Contract
