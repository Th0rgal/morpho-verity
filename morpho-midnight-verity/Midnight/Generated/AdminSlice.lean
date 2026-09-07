-- GENERATED FILE. DO NOT EDIT.
-- Run: node scripts/import_midnight_admin_slice.mjs
import Compiler.CompilationModel

namespace Midnight.Generated.AdminSlice

open Compiler.CompilationModel

structure DeclarationOrigin where
  kind : String
  name : String
  sourceFile : String
  astNodeId : Nat
  sourceSpan : String
  deriving Repr, BEq

def sourceCommit : String := "a7c6da7e70cb216982f6c5d20b46f40b943e67e4"
def compilerVersion : String := "0.8.34+commit.80d5c536"

def fields : List Field := [
    { name := "roleSetterSlot", ty := .address, slot := some 7 },
    { name := "feeSetterSlot", ty := .address, slot := some 8 },
    { name := "feeClaimerSlot", ty := .address, slot := some 9 }
  ]

def errors : List ErrorDef := [
    { name := "OnlyRoleSetter", params := [] }
  ]

def events : List EventDef := [
    { name := "SetRoleSetter", params := [{ name := "roleSetter", ty := .address, kind := .indexed }] },
    { name := "SetFeeSetter", params := [{ name := "feeSetter", ty := .address, kind := .indexed }] },
    { name := "SetFeeClaimer", params := [{ name := "feeClaimer", ty := .address, kind := .indexed }] }
  ]

def functions : List FunctionSpec := [
    { name := "setRoleSetter"
      params := [{ name := "newRoleSetter", ty := .address }]
      returnType := none
      body := [
        .requireError (.eq .caller (.storageAddr "roleSetterSlot")) "OnlyRoleSetter" [],
        .setStorageAddr "roleSetterSlot" (.param "newRoleSetter"),
        .emit "SetRoleSetter" [(.param "newRoleSetter")],
        .stop
      ] },
    { name := "setFeeSetter"
      params := [{ name := "newFeeSetter", ty := .address }]
      returnType := none
      body := [
        .requireError (.eq .caller (.storageAddr "roleSetterSlot")) "OnlyRoleSetter" [],
        .setStorageAddr "feeSetterSlot" (.param "newFeeSetter"),
        .emit "SetFeeSetter" [(.param "newFeeSetter")],
        .stop
      ] },
    { name := "setFeeClaimer"
      params := [{ name := "newFeeClaimer", ty := .address }]
      returnType := none
      body := [
        .requireError (.eq .caller (.storageAddr "roleSetterSlot")) "OnlyRoleSetter" [],
        .setStorageAddr "feeClaimerSlot" (.param "newFeeClaimer"),
        .emit "SetFeeClaimer" [(.param "newFeeClaimer")],
        .stop
      ] }
  ]

def origins : List DeclarationOrigin := [
    { kind := "storage", name := "roleSetter", sourceFile := "src/Midnight.sol", astNodeId := 80, sourceSpan := "13541:25:0" },
    { kind := "storage", name := "feeSetter", sourceFile := "src/Midnight.sol", astNodeId := 82, sourceSpan := "13572:24:0" },
    { kind := "storage", name := "feeClaimer", sourceFile := "src/Midnight.sol", astNodeId := 84, sourceSpan := "13602:25:0" },
    { kind := "error", name := "OnlyRoleSetter", sourceFile := "src/interfaces/IMidnight.sol", astNodeId := 4320, sourceSpan := "2575:23:4" },
    { kind := "event", name := "SetRoleSetter", sourceFile := "src/libraries/EventsLib.sol", astNodeId := 5124, sourceSpan := "354:48:8" },
    { kind := "event", name := "SetFeeSetter", sourceFile := "src/libraries/EventsLib.sol", astNodeId := 5128, sourceSpan := "407:46:8" },
    { kind := "event", name := "SetFeeClaimer", sourceFile := "src/libraries/EventsLib.sol", astNodeId := 5158, sourceSpan := "817:48:8" },
    { kind := "function", name := "setRoleSetter(address)", sourceFile := "src/Midnight.sol", astNodeId := 175, sourceSpan := "14309:212:0" },
    { kind := "function", name := "setFeeSetter(address)", sourceFile := "src/Midnight.sol", astNodeId := 200, sourceSpan := "14527:206:0" },
    { kind := "function", name := "setFeeClaimer(address)", sourceFile := "src/Midnight.sol", astNodeId := 225, sourceSpan := "14739:212:0" }
  ]

end Midnight.Generated.AdminSlice
