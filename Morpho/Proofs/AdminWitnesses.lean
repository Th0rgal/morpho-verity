import Compiler.TypedIRCompilerCorrectness

/-!
# Morpho Blue admin function witness field layouts

Concrete storage layouts and `SupportedStmtList` witnesses for the Morpho Blue
admin patterns (enableIrm, enableLltv, setAuthorization).

Moved from verity's `TypedIRCompilerCorrectness.lean` to keep Morpho-specific
field layouts in the morpho-verity repo.
-/

namespace Morpho.Proofs.AdminWitnesses

open Compiler.CompilationModel
open Verity.Core.Free

/-- Field layout for `enableIrm` witness coverage. -/
def morphoEnableIrmFields : List Field :=
  [ { name := "owner", ty := FieldType.address }
  , { name := "isIrmEnabled", ty := FieldType.mappingTyped (.simple .address) } ]

/-- `owner` resolves to slot 0 as an address field in `morphoEnableIrmFields`. -/
theorem morphoEnableIrmOwnerFieldResolution :
    findFieldWithResolvedSlot morphoEnableIrmFields "owner" =
      some ({ name := "owner", ty := FieldType.address }, 0) := by
  rfl

/-- `isIrmEnabled` resolves to slot 1 in `morphoEnableIrmFields`. -/
theorem morphoEnableIrmMappingFieldResolution :
    findFieldSlot morphoEnableIrmFields "isIrmEnabled" = some 1 := by
  rfl

/-- Concrete witness for mapping-based enablement (`enableIrm`-style). -/
theorem witness_letCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop_enableIrm_supported :
    SupportedStmtList morphoEnableIrmFields
      [ Stmt.letVar "sender" Expr.caller
      , Stmt.letVar "ownerVar" (Expr.storage "owner")
      , Stmt.require (Expr.eq (Expr.localVar "sender") (Expr.localVar "ownerVar")) "not owner"
      , Stmt.letVar "isEnabled" (Expr.mapping "isIrmEnabled" (Expr.param "irm"))
      , Stmt.require (Expr.eq (Expr.localVar "isEnabled") (Expr.literal 0)) "already enabled"
      , Stmt.setMapping "isIrmEnabled" (Expr.param "irm") (Expr.literal 1)
      , Stmt.stop ] := by
  exact ⟨[.letCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop
    "owner" "isIrmEnabled" "sender" "ownerVar" "isEnabled" "irm"
    0 1 1 "not owner" "already enabled"
    morphoEnableIrmOwnerFieldResolution morphoEnableIrmMappingFieldResolution
    (by decide) (by decide) (by decide) (by decide)], rfl⟩

/-- Field layout for `enableLltv` witness coverage. -/
def morphoEnableLltvFields : List Field :=
  [ { name := "owner", ty := FieldType.address }
  , { name := "isLltvEnabled", ty := FieldType.mappingTyped (.simple .uint256) } ]

/-- `owner` resolves to slot 0 as an address field in `morphoEnableLltvFields`. -/
theorem morphoEnableLltvOwnerFieldResolution :
    findFieldWithResolvedSlot morphoEnableLltvFields "owner" =
      some ({ name := "owner", ty := FieldType.address }, 0) := by
  rfl

/-- `isLltvEnabled` resolves to slot 1 in `morphoEnableLltvFields`. -/
theorem morphoEnableLltvMappingFieldResolution :
    findFieldSlot morphoEnableLltvFields "isLltvEnabled" = some 1 := by
  rfl

/-- Concrete witness for mappingUint-based enablement (`enableLltv`-style). -/
theorem witness_letCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop_enableLltv_supported :
    SupportedStmtList morphoEnableLltvFields
      [ Stmt.letVar "sender" Expr.caller
      , Stmt.letVar "ownerVar" (Expr.storage "owner")
      , Stmt.require (Expr.eq (Expr.localVar "sender") (Expr.localVar "ownerVar")) "not owner"
      , Stmt.letVar "isEnabled" (Expr.mappingUint "isLltvEnabled" (Expr.param "lltv"))
      , Stmt.require (Expr.eq (Expr.localVar "isEnabled") (Expr.literal 0)) "already enabled"
      , Stmt.require (Expr.lt (Expr.param "lltv") (Expr.literal 1000000000000000000))
          "lltv too high"
      , Stmt.setMappingUint "isLltvEnabled" (Expr.param "lltv") (Expr.literal 1)
      , Stmt.stop ] := by
  exact ⟨[.letCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop
    "owner" "isLltvEnabled" "sender" "ownerVar" "isEnabled" "lltv"
    0 1 1000000000000000000 1 "not owner" "already enabled" "lltv too high"
    morphoEnableLltvOwnerFieldResolution morphoEnableLltvMappingFieldResolution
    (by decide) (by decide) (by decide) (by decide)], rfl⟩

/-- Field layout for `setAuthorization` witness coverage. -/
def morphoSetAuthorizationFields : List Field :=
  [{ name := "isAuthorized", ty := FieldType.mappingTyped (.nested .address .address) }]

/-- `isAuthorized` resolves to slot 0 in `morphoSetAuthorizationFields`. -/
theorem morphoSetAuthorizationFieldResolution :
    findFieldSlot morphoSetAuthorizationFields "isAuthorized" = some 0 := by
  rfl

/-- Concrete witness for authorization toggle (`setAuthorization`-style). -/
theorem witness_letCallerLetMapping2IteParamReqSetMapping2Stop_setAuthorization_supported :
    SupportedStmtList morphoSetAuthorizationFields
      [ Stmt.letVar "sender" Expr.caller
      , Stmt.letVar "isAuthorizedNow"
          (Expr.mapping2 "isAuthorized" (Expr.localVar "sender") (Expr.param "authorized"))
      , Stmt.ite (Expr.param "newIsAuthorized")
          [ Stmt.require (Expr.eq (Expr.localVar "isAuthorizedNow") (Expr.literal 0))
              "already authorized"
          , Stmt.setMapping2 "isAuthorized" (Expr.localVar "sender") (Expr.param "authorized")
              (Expr.literal 1) ]
          [ Stmt.require
              (Expr.logicalNot (Expr.eq (Expr.localVar "isAuthorizedNow") (Expr.literal 0)))
              "already not authorized"
          , Stmt.setMapping2 "isAuthorized" (Expr.localVar "sender") (Expr.param "authorized")
              (Expr.literal 0) ]
      , Stmt.stop ] := by
  exact ⟨[.letCallerLetMapping2IteParamReqSetMapping2Stop
    "isAuthorized" "sender" "isAuthorizedNow" "authorized" "newIsAuthorized"
    "already authorized" "already not authorized" 0 morphoSetAuthorizationFieldResolution
    (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)], rfl⟩

end Morpho.Proofs.AdminWitnesses
