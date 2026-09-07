import Compiler.CompilationModel
import Midnight.Generated.AdminSlice

namespace Midnight.Compiler.AdminSliceHybrid

open Compiler.CompilationModel

private def expectedSignatures : List (String × List ParamType) := [
  ("setRoleSetter", [.address]),
  ("setFeeSetter", [.address]),
  ("setFeeClaimer", [.address])
]

private def hasSignature (function : FunctionSpec) (signature : String × List ParamType) : Bool :=
  function.name == signature.1 &&
    function.params.map (·.ty) == signature.2 &&
    !function.isInternal

private def functionCount (functions : List FunctionSpec) (signature : String × List ParamType) : Nat :=
  (functions.filter fun function => hasSignature function signature).length

private def sameField (left right : Field) : Bool :=
  left.name == right.name &&
    left.ty == right.ty &&
    left.slot == right.slot &&
    left.packedBits == right.packedBits &&
    left.aliasSlots == right.aliasSlots

private def sameEventParam (left right : EventParam) : Bool :=
  left.name == right.name && left.ty == right.ty && left.kind == right.kind

private def sameEvent (left right : EventDef) : Bool :=
  left.name == right.name &&
    left.params.length == right.params.length &&
    (left.params.zip right.params).all fun (leftParam, rightParam) =>
      sameEventParam leftParam rightParam

private def validateFields (base : CompilationModel) : Except String Unit := do
  for imported in Midnight.Generated.AdminSlice.fields do
    match base.fields.filter (fun field => field.name == imported.name) with
    | [existing] =>
        if !sameField existing imported then
          throw s!"Midnight admin import: storage/type conflict for {imported.name}"
    | others =>
        throw s!"Midnight admin import: expected one handwritten field {imported.name}, found {others.length}"

private def validateErrors (base : CompilationModel) : Except String Unit := do
  for imported in Midnight.Generated.AdminSlice.errors do
    match base.errors.filter (fun error => error.name == imported.name) with
    | [existing] =>
        if existing.params != imported.params then
          throw s!"Midnight admin import: custom-error conflict for {imported.name}"
    | others =>
        throw s!"Midnight admin import: expected one handwritten error {imported.name}, found {others.length}"

private def mergeEvents (base : CompilationModel) : Except String (List EventDef) := do
  let mut merged := base.events
  for imported in Midnight.Generated.AdminSlice.events do
    match merged.filter (fun event => event.name == imported.name) with
    | [] => merged := merged ++ [imported]
    | [existing] =>
        if !sameEvent existing imported then
          throw s!"Midnight admin import: event conflict for {imported.name}"
    | others =>
        throw s!"Midnight admin import: duplicate event {imported.name} ({others.length} declarations)"
  pure merged

private def validateFunctionSets (base : CompilationModel) : Except String Unit := do
  for signature in expectedSignatures do
    let handwrittenCount := functionCount base.functions signature
    if handwrittenCount != 1 then
      throw s!"Midnight admin import: expected one handwritten replacement for {signature.1}, found {handwrittenCount}"
    let importedCount := functionCount Midnight.Generated.AdminSlice.functions signature
    if importedCount != 1 then
      throw s!"Midnight admin import: expected one imported function for {signature.1}, found {importedCount}"
  if Midnight.Generated.AdminSlice.functions.length != expectedSignatures.length then
    throw "Midnight admin import: generated function set contains an unexpected declaration"

private def replaceFunctions (base : CompilationModel) : Except String (List FunctionSpec) := do
  base.functions.mapM fun function =>
    match expectedSignatures.find? (hasSignature function) with
    | none => pure function
    | some signature =>
        match Midnight.Generated.AdminSlice.functions.find? (fun imported => hasSignature imported signature) with
        | none => throw s!"Midnight admin import: missing replacement for {signature.1}"
        | some imported => pure imported

/--
Replace the three handwritten public admin functions with the deterministic
Sol-C AST import. The rest of the handwritten Midnight model remains unchanged.
Storage/error declarations must agree with the existing model; imported events
are added only when absent and conflicts fail closed.
-/
def assemble (base : CompilationModel) : Except String CompilationModel := do
  validateFields base
  validateErrors base
  validateFunctionSets base
  let functions ← replaceFunctions base
  let events ← mergeEvents base
  for signature in expectedSignatures do
    let count := functionCount functions signature
    if count != 1 then
      throw s!"Midnight admin import: hybrid artifact has {count} copies of {signature.1}"
  pure { base with functions, events }

end Midnight.Compiler.AdminSliceHybrid
