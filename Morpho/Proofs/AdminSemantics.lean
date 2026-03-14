import Compiler.TypedIRCompilerCorrectness

/-!
# Morpho Blue admin function base semantics and correctness

Source/compiled semantics definitions and semantic-preservation theorems for
the five Morpho Blue admin function patterns:
1. setOwner / setFeeRecipient (owner-check, set storage addr)
2. setFeeRecipient owner-auth (two storage reads, owner-check, set storage addr)
3. enableIrm (owner-check, mapping read, mapping write)
4. enableLltv (owner-check, mappingUint read, bound check, mappingUint write)
5. setAuthorization (caller, mapping2 read, ite bool, require, setMapping2)

Moved from verity's `TypedIRCompilerCorrectness.lean` to keep Morpho-specific
proof artifacts in the morpho-verity repo.
-/

namespace Morpho.Proofs.AdminSemantics

open Compiler.CompilationModel
open Verity.Core.Free

-- ============================================================================
-- Morpho Blue admin function base semantics definitions
-- ============================================================================

/-- Source semantics for the Morpho setOwner/setFeeRecipient pattern.
Params: paramName (address, id 0).
Locals: senderVar (address, id 1), ownerVar (address, id 2). -/
def execSourceLetCallerLetStorageAddrReqEqReqNeqSetStorageAddrParamStop
    (init : TExecState) (ownerSlot : Nat) (msg1 msg2 : String) : TExecResult :=
  let sender := init.env.sender
  let owner := init.world.storageAddr ownerSlot
  let param := init.vars.address 0
  if decide (sender = owner) then
    if decide (¬ (param = owner)) then
      .ok { init with
        vars := TVars.set (TVars.set init.vars
          { id := 1, ty := Ty.address } sender)
          { id := 2, ty := Ty.address } owner,
        world := { init.world with
          storageAddr := fun i => if i == ownerSlot then param else init.world.storageAddr i } }
    else .revert msg2
  else .revert msg1

/-- Compiled semantics for the Morpho setOwner/setFeeRecipient pattern.
Pre-populated CompileState: paramName (address, id 0). -/
def execCompiledLetCallerLetStorageAddrReqEqReqNeqSetStorageAddrParamStop
    (fields : List Field) (ownerField senderVar ownerVar paramName : String)
    (msg1 msg2 : String) (init : TExecState) : TExecResult :=
  match (compileStmts fields
      [ Stmt.letVar senderVar Expr.caller
      , Stmt.letVar ownerVar (Expr.storage ownerField)
      , Stmt.require (Expr.eq (Expr.localVar senderVar) (Expr.localVar ownerVar)) msg1
      , Stmt.require (Expr.logicalNot (Expr.eq (Expr.param paramName) (Expr.localVar ownerVar))) msg2
      , Stmt.setStorage ownerField (Expr.param paramName)
      , Stmt.stop
      ]).run
      (CompileState.mk 1
        [(paramName, { id := 0, ty := Ty.address })]
        #[{ id := 0, ty := Ty.address }]
        #[] #[]) with
  | .error err => .revert err
  | .ok (_, st) => evalTStmts init st.body.toList

/-- Source semantics for the Morpho setFeeRecipient owner-auth pattern.
Params: paramName (address, id 0).
Locals: senderVar (address, id 1), ownerVar (address, id 2), targetVar (address, id 3). -/
def execSourceLetCallerLetStorageAddrReqEqLetStorageAddrReqNeqSetStorageAddrParamStop
    (init : TExecState) (ownerSlot targetSlot : Nat) (msg1 msg2 : String) : TExecResult :=
  let sender := init.env.sender
  let owner := init.world.storageAddr ownerSlot
  let param := init.vars.address 0
  if decide (sender = owner) then
    let targetVal := init.world.storageAddr targetSlot
    if decide (¬ (param = targetVal)) then
      .ok { init with
        vars := TVars.set (TVars.set (TVars.set init.vars
          { id := 1, ty := Ty.address } sender)
          { id := 2, ty := Ty.address } owner)
          { id := 3, ty := Ty.address } targetVal,
        world := { init.world with
          storageAddr := fun i => if i == targetSlot then param else init.world.storageAddr i } }
    else .revert msg2
  else .revert msg1

/-- Compiled semantics for the Morpho setFeeRecipient owner-auth pattern.
Pre-populated CompileState: paramName (address, id 0). -/
def execCompiledLetCallerLetStorageAddrReqEqLetStorageAddrReqNeqSetStorageAddrParamStop
    (fields : List Field) (ownerField targetField senderVar ownerVar targetVar paramName : String)
    (msg1 msg2 : String) (init : TExecState) : TExecResult :=
  match (compileStmts fields
      (morphoSetFeeRecipientOwnerAuthStmts
        ownerField targetField senderVar ownerVar targetVar paramName msg1 msg2)).run
      (CompileState.mk 1
        [(paramName, { id := 0, ty := Ty.address })]
        #[{ id := 0, ty := Ty.address }]
        #[] #[]) with
  | .error err => .revert err
  | .ok (_, st) => evalTStmts init st.body.toList

/-- Source semantics for the Morpho enableIrm pattern.
Params: keyParam (address, id 0).
Locals: senderVar (address, id 1), ownerVar (address, id 2), currentVar (uint256, id 3). -/
def execSourceLetCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop
    (init : TExecState) (ownerSlot mappingSlot : Nat) (writeVal : Nat)
    (msg1 msg2 : String) : TExecResult :=
  let sender := init.env.sender
  let owner := init.world.storageAddr ownerSlot
  let key := init.vars.address 0
  if decide (sender = owner) then
    let currentVal := init.world.storageMap mappingSlot key
    if decide ((currentVal : Verity.Core.Uint256) = (0 : Verity.Core.Uint256)) then
      .ok { init with
        vars := TVars.set (TVars.set (TVars.set init.vars
          { id := 1, ty := Ty.address } sender)
          { id := 2, ty := Ty.address } owner)
          { id := 3, ty := Ty.uint256 } currentVal,
        world := { init.world with
          storageMap := fun i a => if i == mappingSlot && a == key
            then (writeVal : Verity.Core.Uint256)
            else init.world.storageMap i a } }
    else .revert msg2
  else .revert msg1

/-- Compiled semantics for the Morpho enableIrm pattern.
Pre-populated CompileState: keyParam (address, id 0). -/
def execCompiledLetCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop
    (fields : List Field) (ownerField mappingField senderVar ownerVar currentVar keyParam : String)
    (writeVal : Nat) (msg1 msg2 : String) (init : TExecState) : TExecResult :=
  match (compileStmts fields
      [ Stmt.letVar senderVar Expr.caller
      , Stmt.letVar ownerVar (Expr.storage ownerField)
      , Stmt.require (Expr.eq (Expr.localVar senderVar) (Expr.localVar ownerVar)) msg1
      , Stmt.letVar currentVar (Expr.mapping mappingField (Expr.param keyParam))
      , Stmt.require (Expr.eq (Expr.localVar currentVar) (Expr.literal 0)) msg2
      , Stmt.setMapping mappingField (Expr.param keyParam) (Expr.literal writeVal)
      , Stmt.stop
      ]).run
      (CompileState.mk 1
        [(keyParam, { id := 0, ty := Ty.address })]
        #[{ id := 0, ty := Ty.address }]
        #[] #[]) with
  | .error err => .revert err
  | .ok (_, st) => evalTStmts init st.body.toList

/-- Source semantics for the Morpho enableLltv pattern.
Params: keyParam (uint256, id 0).
Locals: senderVar (address, id 1), ownerVar (address, id 2), currentVar (uint256, id 3). -/
def execSourceLetCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop
    (init : TExecState) (ownerSlot mappingSlot : Nat) (bound writeVal : Nat)
    (msg1 msg2 msg3 : String) : TExecResult :=
  let sender := init.env.sender
  let owner := init.world.storageAddr ownerSlot
  let key := init.vars.uint256 0
  if decide (sender = owner) then
    let currentVal := init.world.storageMapUint mappingSlot key
    if decide ((currentVal : Verity.Core.Uint256) = (0 : Verity.Core.Uint256)) then
      if decide ((key : Verity.Core.Uint256) < (bound : Verity.Core.Uint256)) then
        .ok { init with
          vars := TVars.set (TVars.set (TVars.set init.vars
            { id := 1, ty := Ty.address } sender)
            { id := 2, ty := Ty.address } owner)
            { id := 3, ty := Ty.uint256 } currentVal,
          world := { init.world with
            storageMapUint := fun i k => if i == mappingSlot && k == key
              then (writeVal : Verity.Core.Uint256)
              else init.world.storageMapUint i k } }
      else .revert msg3
    else .revert msg2
  else .revert msg1

/-- Compiled semantics for the Morpho enableLltv pattern.
Pre-populated CompileState: keyParam (uint256, id 0). -/
def execCompiledLetCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop
    (fields : List Field) (ownerField mappingField senderVar ownerVar currentVar keyParam : String)
    (bound writeVal : Nat) (msg1 msg2 msg3 : String) (init : TExecState) : TExecResult :=
  match (compileStmts fields
      [ Stmt.letVar senderVar Expr.caller
      , Stmt.letVar ownerVar (Expr.storage ownerField)
      , Stmt.require (Expr.eq (Expr.localVar senderVar) (Expr.localVar ownerVar)) msg1
      , Stmt.letVar currentVar (Expr.mappingUint mappingField (Expr.param keyParam))
      , Stmt.require (Expr.eq (Expr.localVar currentVar) (Expr.literal 0)) msg2
      , Stmt.require (Expr.lt (Expr.param keyParam) (Expr.literal bound)) msg3
      , Stmt.setMappingUint mappingField (Expr.param keyParam) (Expr.literal writeVal)
      , Stmt.stop
      ]).run
      (CompileState.mk 1
        [(keyParam, { id := 0, ty := Ty.uint256 })]
        #[{ id := 0, ty := Ty.uint256 }]
        #[] #[]) with
  | .error err => .revert err
  | .ok (_, st) => evalTStmts init st.body.toList

/-- Source semantics for the Morpho setAuthorization pattern.
Params: authParam (address, id 0), boolParam (bool, id 1).
Locals: senderVar (address, id 2), currentVar (uint256, id 3). -/
def execSourceLetCallerLetMapping2IteParamReqSetMapping2Stop
    (init : TExecState) (mappingSlot : Nat) (msg1 msg2 : String) : TExecResult :=
  let sender := init.env.sender
  let authAddr := init.vars.address 0
  let currentVal := init.world.storageMap2 mappingSlot sender authAddr
  if init.vars.bool 1 then
    if decide ((currentVal : Verity.Core.Uint256) = (0 : Verity.Core.Uint256)) then
      .ok { init with
        vars := TVars.set (TVars.set init.vars
          { id := 2, ty := Ty.address } sender)
          { id := 3, ty := Ty.uint256 } currentVal,
        world := { init.world with
          storageMap2 := fun i a1 a2 => if i == mappingSlot && a1 == sender && a2 == authAddr
            then (1 : Verity.Core.Uint256)
            else init.world.storageMap2 i a1 a2 } }
    else .revert msg1
  else
    if decide (¬ ((currentVal : Verity.Core.Uint256) = (0 : Verity.Core.Uint256))) then
      .ok { init with
        vars := TVars.set (TVars.set init.vars
          { id := 2, ty := Ty.address } sender)
          { id := 3, ty := Ty.uint256 } currentVal,
        world := { init.world with
          storageMap2 := fun i a1 a2 => if i == mappingSlot && a1 == sender && a2 == authAddr
            then (0 : Verity.Core.Uint256)
            else init.world.storageMap2 i a1 a2 } }
    else .revert msg2

/-- Compiled semantics for the Morpho setAuthorization pattern.
Pre-populated CompileState: authParam (address, id 0), boolParam (bool, id 1). -/
def execCompiledLetCallerLetMapping2IteParamReqSetMapping2Stop
    (fields : List Field) (mappingField senderVar currentVar authParam boolParam : String)
    (msg1 msg2 : String) (init : TExecState) : TExecResult :=
  match (compileStmts fields
      [ Stmt.letVar senderVar Expr.caller
      , Stmt.letVar currentVar (Expr.mapping2 mappingField (Expr.localVar senderVar) (Expr.param authParam))
      , Stmt.ite (Expr.param boolParam)
          [ Stmt.require (Expr.eq (Expr.localVar currentVar) (Expr.literal 0)) msg1
          , Stmt.setMapping2 mappingField (Expr.localVar senderVar) (Expr.param authParam) (Expr.literal 1)
          ]
          [ Stmt.require (Expr.logicalNot (Expr.eq (Expr.localVar currentVar) (Expr.literal 0))) msg2
          , Stmt.setMapping2 mappingField (Expr.localVar senderVar) (Expr.param authParam) (Expr.literal 0)
          ]
      , Stmt.stop
      ]).run
      (CompileState.mk 2
        [(boolParam, { id := 1, ty := Ty.bool }),
         (authParam, { id := 0, ty := Ty.address })]
        #[{ id := 0, ty := Ty.address }, { id := 1, ty := Ty.bool }]
        #[] #[]) with
  | .error err => .revert err
  | .ok (_, st) => evalTStmts init st.body.toList

-- ============================================================================
-- Morpho Blue admin function correctness theorems
-- ============================================================================

/-- Semantic-preservation for the Morpho setOwner/setFeeRecipient pattern. -/
theorem compile_letCaller_letStorageAddr_reqEq_reqNeq_setStorageAddr_param_stop_semantics
    (fields : List Field) (ownerField senderVar ownerVar paramName msg1 msg2 : String)
    (ownerSlot : Nat) (init : TExecState)
    (hOwner : findFieldWithResolvedSlot fields ownerField =
      some ({ name := ownerField, ty := FieldType.address }, ownerSlot))
    (hne_sv_p : senderVar ≠ paramName)
    (hne_ov_p : ownerVar ≠ paramName)
    (hne_ov_sv : ownerVar ≠ senderVar) :
    execCompiledLetCallerLetStorageAddrReqEqReqNeqSetStorageAddrParamStop
        fields ownerField senderVar ownerVar paramName msg1 msg2 init =
      execSourceLetCallerLetStorageAddrReqEqReqNeqSetStorageAddrParamStop
        init ownerSlot msg1 msg2 := by
  simp [execCompiledLetCallerLetStorageAddrReqEqReqNeqSetStorageAddrParamStop,
    execSourceLetCallerLetStorageAddrReqEqReqNeqSetStorageAddrParamStop,
    compileStmts_letCallerLetStorageAddrReqEqReqNeqSetStorageAddrParamStop_run,
    hOwner, hne_sv_p, hne_ov_p, hne_ov_sv,
    evalTStmts, defaultEvalFuel]
  by_cases hEq1 : init.env.sender = init.world.storageAddr ownerSlot
  · by_cases hEq2 : init.vars.address 0 = init.world.storageAddr ownerSlot
    · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, hEq2, TVars.set, TVars.get]
    · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, hEq2, TVars.set, TVars.get]
  · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, TVars.set, TVars.get]

/-- Semantic-preservation for the Morpho setFeeRecipient owner-auth pattern. -/
theorem compile_letCaller_letStorageAddr_reqEq_letStorageAddr_reqNeq_setStorageAddr_param_stop_semantics
    (fields : List Field) (ownerField targetField senderVar ownerVar targetVar paramName msg1 msg2 : String)
    (ownerSlot targetSlot : Nat) (init : TExecState)
    (hOwner : findFieldWithResolvedSlot fields ownerField =
      some ({ name := ownerField, ty := FieldType.address }, ownerSlot))
    (hTarget : findFieldWithResolvedSlot fields targetField =
      some ({ name := targetField, ty := FieldType.address }, targetSlot))
    (hne_sv_p : senderVar ≠ paramName)
    (hne_ov_p : ownerVar ≠ paramName)
    (hne_ov_sv : ownerVar ≠ senderVar)
    (hne_tv_p : targetVar ≠ paramName)
    (hne_tv_sv : targetVar ≠ senderVar)
    (hne_tv_ov : targetVar ≠ ownerVar) :
    execCompiledLetCallerLetStorageAddrReqEqLetStorageAddrReqNeqSetStorageAddrParamStop
        fields ownerField targetField senderVar ownerVar targetVar paramName msg1 msg2 init =
      execSourceLetCallerLetStorageAddrReqEqLetStorageAddrReqNeqSetStorageAddrParamStop
        init ownerSlot targetSlot msg1 msg2 := by
  simp [execCompiledLetCallerLetStorageAddrReqEqLetStorageAddrReqNeqSetStorageAddrParamStop,
    execSourceLetCallerLetStorageAddrReqEqLetStorageAddrReqNeqSetStorageAddrParamStop,
    compileStmts_letCallerLetStorageAddrReqEqLetStorageAddrReqNeqSetStorageAddrParamStop_run,
    hOwner, hTarget, hne_sv_p, hne_ov_p, hne_ov_sv, hne_tv_p, hne_tv_sv, hne_tv_ov,
    evalTStmts, defaultEvalFuel]
  by_cases hEq1 : init.env.sender = init.world.storageAddr ownerSlot
  · by_cases hEq2 : init.vars.address 0 = init.world.storageAddr targetSlot
    · simp [morphoSetFeeRecipientOwnerAuthExpectedState, evalTStmtsFuel, evalTStmtFuel, evalTExpr,
        hEq1, hEq2, TVars.set, TVars.get]
    · simp [morphoSetFeeRecipientOwnerAuthExpectedState, evalTStmtsFuel, evalTStmtFuel, evalTExpr,
        hEq1, hEq2, TVars.set, TVars.get]
  · simp [morphoSetFeeRecipientOwnerAuthExpectedState, evalTStmtsFuel, evalTStmtFuel, evalTExpr,
      hEq1, TVars.set, TVars.get]

/-- Semantic-preservation for the Morpho enableIrm pattern. -/
theorem compile_letCaller_letStorageAddr_reqEq_letMapping_reqEqLit_setMapping_stop_semantics
    (fields : List Field) (ownerField mappingField senderVar ownerVar currentVar keyParam : String)
    (ownerSlot mappingSlot : Nat) (writeVal : Nat) (init : TExecState) (msg1 msg2 : String)
    (hOwner : findFieldWithResolvedSlot fields ownerField =
      some ({ name := ownerField, ty := FieldType.address }, ownerSlot))
    (hMapping : findFieldSlot fields mappingField = some mappingSlot)
    (hne_sv_kp : senderVar ≠ keyParam)
    (hne_ov_kp : ownerVar ≠ keyParam)
    (hne_ov_sv : ownerVar ≠ senderVar)
    (hne_cv_kp : currentVar ≠ keyParam) :
    execCompiledLetCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop
        fields ownerField mappingField senderVar ownerVar currentVar keyParam writeVal msg1 msg2 init =
      execSourceLetCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop
        init ownerSlot mappingSlot writeVal msg1 msg2 := by
  simp [execCompiledLetCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop,
    execSourceLetCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop,
    compileStmts_letCallerLetStorageAddrReqEqLetMappingReqEqLitSetMappingStop_run,
    hOwner, hMapping, hne_sv_kp, hne_ov_kp, hne_ov_sv, hne_cv_kp,
    evalTStmts, defaultEvalFuel]
  by_cases hEq1 : init.env.sender = init.world.storageAddr ownerSlot
  · by_cases hEq2 : init.world.storageMap mappingSlot (init.vars.address 0) = (0 : Verity.Core.Uint256)
    · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, hEq2, TVars.set, TVars.get]
    · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, hEq2, TVars.set, TVars.get]
  · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, TVars.set, TVars.get]

/-- Semantic-preservation for the Morpho enableLltv pattern. -/
theorem compile_letCaller_letStorageAddr_reqEq_letMappingUint_reqEqLit_reqLt_setMappingUint_stop_semantics
    (fields : List Field) (ownerField mappingField senderVar ownerVar currentVar keyParam : String)
    (ownerSlot mappingSlot : Nat) (bound writeVal : Nat) (init : TExecState)
    (msg1 msg2 msg3 : String)
    (hOwner : findFieldWithResolvedSlot fields ownerField =
      some ({ name := ownerField, ty := FieldType.address }, ownerSlot))
    (hMapping : findFieldSlot fields mappingField = some mappingSlot)
    (hne_sv_kp : senderVar ≠ keyParam)
    (hne_ov_kp : ownerVar ≠ keyParam)
    (hne_ov_sv : ownerVar ≠ senderVar)
    (hne_cv_kp : currentVar ≠ keyParam) :
    execCompiledLetCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop
        fields ownerField mappingField senderVar ownerVar currentVar keyParam bound writeVal
        msg1 msg2 msg3 init =
      execSourceLetCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop
        init ownerSlot mappingSlot bound writeVal msg1 msg2 msg3 := by
  simp [execCompiledLetCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop,
    execSourceLetCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop,
    compileStmts_letCallerLetStorageAddrReqEqLetMappingUintReqEqLitReqLtSetMappingUintStop_run,
    hOwner, hMapping, hne_sv_kp, hne_ov_kp, hne_ov_sv, hne_cv_kp,
    evalTStmts, defaultEvalFuel]
  by_cases hEq1 : init.env.sender = init.world.storageAddr ownerSlot
  · by_cases hEq2 : init.world.storageMapUint mappingSlot (init.vars.uint256 0) = (0 : Verity.Core.Uint256)
    · by_cases hEq3 : (init.vars.uint256 0 : Verity.Core.Uint256) < (bound : Verity.Core.Uint256)
      · have hNat : (init.vars.uint256 0).val < bound % Verity.Core.Uint256.modulus := by
          simpa [Verity.Core.Uint256.lt_def, Verity.Core.Uint256.val_ofNat] using hEq3
        simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, hEq2, hEq3, hNat, TVars.set, TVars.get]
      · have hNat : bound % Verity.Core.Uint256.modulus ≤ (init.vars.uint256 0).val :=
          Nat.not_lt.mp (by simpa [Verity.Core.Uint256.lt_def, Verity.Core.Uint256.val_ofNat] using hEq3)
        simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, hEq2, hEq3, hNat, TVars.set, TVars.get]
    · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, hEq2, TVars.set, TVars.get]
  · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hEq1, TVars.set, TVars.get]

/-- Semantic-preservation for the Morpho setAuthorization pattern. -/
theorem compile_letCaller_letMapping2_ite_param_req_setMapping2_stop_semantics
    (fields : List Field) (mappingField senderVar currentVar authParam boolParam : String)
    (mappingSlot : Nat) (init : TExecState) (msg1 msg2 : String)
    (hMapping : findFieldSlot fields mappingField = some mappingSlot)
    (hne_sv_bp : senderVar ≠ boolParam)
    (hne_sv_ap : senderVar ≠ authParam)
    (hne_cv_bp : currentVar ≠ boolParam)
    (hne_cv_ap : currentVar ≠ authParam)
    (hne_cv_sv : currentVar ≠ senderVar)
    (hne_bp_ap : boolParam ≠ authParam) :
    execCompiledLetCallerLetMapping2IteParamReqSetMapping2Stop
        fields mappingField senderVar currentVar authParam boolParam msg1 msg2 init =
      execSourceLetCallerLetMapping2IteParamReqSetMapping2Stop
        init mappingSlot msg1 msg2 := by
  simp [execCompiledLetCallerLetMapping2IteParamReqSetMapping2Stop,
    execSourceLetCallerLetMapping2IteParamReqSetMapping2Stop,
    compileStmts_letCallerLetMapping2IteParamReqSetMapping2Stop_run,
    hMapping, hne_sv_bp, hne_sv_ap, hne_cv_bp, hne_cv_ap, hne_cv_sv, hne_bp_ap,
    evalTStmts, defaultEvalFuel]
  by_cases hBool : init.vars.bool 1
  · by_cases hEq : init.world.storageMap2 mappingSlot init.env.sender (init.vars.address 0) = (0 : Verity.Core.Uint256)
    · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hBool, hEq, TVars.set, TVars.get]
    · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hBool, hEq, TVars.set, TVars.get]
  · by_cases hEq : init.world.storageMap2 mappingSlot init.env.sender (init.vars.address 0) = (0 : Verity.Core.Uint256)
    · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hBool, hEq, TVars.set, TVars.get]
    · simp [evalTStmtsFuel, evalTStmtFuel, evalTExpr, hBool, hEq, TVars.set, TVars.get]

end Morpho.Proofs.AdminSemantics
