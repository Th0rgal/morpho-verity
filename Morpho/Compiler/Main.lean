import Std
import Compiler.CompilationModel
import Compiler.Codegen
import Compiler.Yul.PrettyPrint
import Compiler.Linker
import Compiler.ParityPacks
import Compiler.Lowering.FromEDSL
import Compiler.ABI
import Morpho.Compiler.Spec
import Morpho.Compiler.Generated

namespace Morpho.Compiler.Main

open _root_.Compiler
open _root_.Compiler.CompilationModel
open _root_.Compiler.Yul
open _root_.Compiler.Linker

private def orThrow {α : Type} (r : Except String α) : IO α :=
  match r with
  | .ok v => pure v
  | .error e => throw (IO.userError e)

private structure CLIArgs where
  outDir : String := "artifacts/yul"
  abiOutDir : Option String := none
  libs : List String := []
  verbose : Bool := false
  backendProfile : _root_.Compiler.BackendProfile := .semantic
  backendProfileExplicit : Bool := false
  parityPackId : Option String := none
  patchEnabled : Bool := false
  patchMaxIterations : Nat := 2
  patchMaxIterationsExplicit : Bool := false
  rewriteBundleId : String := _root_.Compiler.Yul.foundationRewriteBundleId
  requiredProofRefs : List String := []
  mappingSlotScratchBase : Nat := 0x200

private def parseBackendProfile (raw : String) : Option _root_.Compiler.BackendProfile :=
  match raw with
  | "semantic" => some .semantic
  | "solidity-parity-ordering" => some .solidityParityOrdering
  | "solidity-parity" => some .solidityParity
  | _ => none

private def backendProfileString (profile : _root_.Compiler.BackendProfile) : String :=
  match profile with
  | .semantic => "semantic"
  | .solidityParityOrdering => "solidity-parity-ordering"
  | .solidityParity => "solidity-parity"

private def profileForcesPatches (profile : _root_.Compiler.BackendProfile) : Bool :=
  match profile with
  | .solidityParity => true
  | _ => false

private def packForcesPatches (cfg : CLIArgs) : Bool :=
  match cfg.parityPackId with
  | some packId =>
      match _root_.Compiler.findParityPack? packId with
      | some pack => pack.forcePatches
      | none => false
  | none => false

private def patchEnabledFor (cfg : CLIArgs) : Bool :=
  cfg.patchEnabled || profileForcesPatches cfg.backendProfile || packForcesPatches cfg

private def parseArgs (args : List String) : IO CLIArgs :=
  let rec go : List String → CLIArgs → IO CLIArgs
    | [], cfg => pure { cfg with libs := cfg.libs.reverse }
    | "--output" :: dir :: rest, cfg => go rest { cfg with outDir := dir }
    | "-o" :: dir :: rest, cfg => go rest { cfg with outDir := dir }
    | "--abi-output" :: dir :: rest, cfg => go rest { cfg with abiOutDir := some dir }
    | "--link" :: path :: rest, cfg => go rest { cfg with libs := path :: cfg.libs }
    | ["--output"], _ | ["-o"], _ =>
        throw (IO.userError "Missing value for --output")
    | ["--abi-output"], _ =>
        throw (IO.userError "Missing value for --abi-output")
    | ["--link"], _ =>
        throw (IO.userError "Missing value for --link")
    | "--backend-profile" :: raw :: rest, cfg =>
        if cfg.parityPackId.isSome then
          throw (IO.userError "Cannot combine --backend-profile with --parity-pack")
        else
          match parseBackendProfile raw with
          | some profile => go rest { cfg with backendProfile := profile, backendProfileExplicit := true }
          | none =>
              throw (IO.userError
                s!"Invalid value for --backend-profile: {raw} (expected semantic, solidity-parity-ordering, or solidity-parity)")
    | ["--backend-profile"], _ =>
        throw (IO.userError "Missing value for --backend-profile")
    | "--parity-pack" :: raw :: rest, cfg =>
        if cfg.backendProfileExplicit then
          throw (IO.userError "Cannot combine --parity-pack with --backend-profile")
        else
          match _root_.Compiler.findParityPack? raw with
          | some pack =>
              if !pack.proofCompositionValid then
                throw (IO.userError
                  s!"Invalid parity pack proof composition: {pack.id}")
              else
              go rest {
                cfg with
                  parityPackId := some pack.id
                  backendProfile := pack.backendProfile
                  patchEnabled := cfg.patchEnabled || pack.forcePatches
                  patchMaxIterations :=
                    if cfg.patchMaxIterationsExplicit then cfg.patchMaxIterations else pack.defaultPatchMaxIterations
                  rewriteBundleId := pack.rewriteBundleId
                  requiredProofRefs := pack.requiredProofRefs
              }
          | none =>
              throw (IO.userError
                s!"Invalid value for --parity-pack: {raw} (supported: {String.intercalate ", " _root_.Compiler.supportedParityPackIds})")
    | ["--parity-pack"], _ =>
        throw (IO.userError "Missing value for --parity-pack")
    | "--enable-patches" :: rest, cfg =>
        go rest { cfg with patchEnabled := true }
    | "--patch-max-iterations" :: raw :: rest, cfg =>
        match raw.toNat? with
        | some n => go rest { cfg with patchEnabled := true, patchMaxIterations := n, patchMaxIterationsExplicit := true }
        | none => throw (IO.userError s!"Invalid value for --patch-max-iterations: {raw}")
    | ["--patch-max-iterations"], _ =>
        throw (IO.userError "Missing value for --patch-max-iterations")
    | "--mapping-slot-scratch-base" :: raw :: rest, cfg =>
        match raw.toNat? with
        | some n => go rest { cfg with mappingSlotScratchBase := n }
        | none => throw (IO.userError s!"Invalid value for --mapping-slot-scratch-base: {raw}")
    | ["--mapping-slot-scratch-base"], _ =>
        throw (IO.userError "Missing value for --mapping-slot-scratch-base")
    | "--verbose" :: rest, cfg => go rest { cfg with verbose := true }
    | "-v" :: rest, cfg => go rest { cfg with verbose := true }
    | "--help" :: _, _
    | "-h" :: _, _ => do
      IO.println "Usage: morpho-verity-compiler [options]"
      IO.println ""
      IO.println "Options:"
      IO.println "  --output <dir>, -o <dir>    Output directory (default: artifacts/yul)"
      IO.println "  --abi-output <dir>          Output ABI JSON artifact (<Contract>.abi.json)"
      IO.println "  (EDSL input boundary is the only supported mode.)"
      IO.println "  --link <path>               Link external Yul library (repeatable)"
      IO.println "  --backend-profile <semantic|solidity-parity-ordering|solidity-parity>"
      IO.println "  --parity-pack <id>          Versioned parity pack from verity"
      IO.println "  --enable-patches            Enable deterministic Yul patch pass"
      IO.println "  --patch-max-iterations <n>  Max patch-pass fixpoint iterations (default: 2)"
      IO.println "  --mapping-slot-scratch-base <n>  Scratch memory base for mappingSlot helper (default: 512)"
      IO.println "  --verbose, -v               Verbose logs"
      IO.println "  --help, -h                  Show this help"
      throw (IO.userError "help")
    | x :: _, _ =>
      throw (IO.userError s!"Unknown argument: {x}")
  go args {}

private def morphoEmitOptions (cfg : CLIArgs) : _root_.Compiler.YulEmitOptions :=
  { backendProfile := cfg.backendProfile
    patchConfig := {
      enabled := patchEnabledFor cfg
      maxIterations := cfg.patchMaxIterations
      packId := cfg.parityPackId.getD ""
      rewriteBundleId := cfg.rewriteBundleId
      requiredProofRefs := cfg.requiredProofRefs
    }
    mappingSlotScratchBase := cfg.mappingSlotScratchBase }

private def lowerMorphoGeneratedSpec : IO _root_.Compiler.CompilationModel.CompilationModel := do
  let lowered := _root_.Compiler.Lowering.lowerModelPath Morpho.Compiler.Generated.morphoGeneratedSpec
  match lowered with
  | .ok spec => pure spec
  | .error err => throw (IO.userError err.message)

private def resolveMorphoGeneratedSelectors : IO (List Nat) := do
  Morpho.Compiler.Generated.morphoGeneratedSelectors

private def writeContract
    (outDir : String)
    (contract : IRContract)
    (libraryPaths : List String)
    (options : _root_.Compiler.YulEmitOptions) : IO Unit := do
  let yulObj := _root_.Compiler.emitYulWithOptions contract options
  let libraries ← libraryPaths.mapM loadLibrary
  let allLibFunctions := libraries.flatten
  let linkedLibFunctions := selectDirectlyReferencedLibraries yulObj allLibFunctions

  if !linkedLibFunctions.isEmpty then
    let _ ← orThrow (validateNoDuplicateNames linkedLibFunctions)
    let _ ← orThrow (validateNoNameCollisions yulObj linkedLibFunctions)
  let _ ← orThrow (validateExternalReferences yulObj linkedLibFunctions)
  if !linkedLibFunctions.isEmpty then
    let _ ← orThrow (validateCallArity yulObj linkedLibFunctions)

  let text ←
    if linkedLibFunctions.isEmpty then
      pure (_root_.Compiler.Yul.render yulObj)
    else
      orThrow (renderWithLibraries yulObj linkedLibFunctions)

  IO.FS.createDirAll outDir
  IO.FS.writeFile s!"{outDir}/{contract.name}.yul" text

def main (args : List String) : IO Unit := do
  try
    let cfg ← parseArgs args
    if cfg.verbose then
      IO.println s!"Compiling Morpho generated spec to {cfg.outDir}"
      match cfg.abiOutDir with
      | some dir => IO.println s!"ABI output directory: {dir}"
      | none => pure ()
      IO.println "Input mode: edsl"
      IO.println s!"Backend profile: {backendProfileString cfg.backendProfile}"
      match cfg.parityPackId with
      | some packId =>
          IO.println s!"Parity pack: {packId}"
          match _root_.Compiler.findParityPack? packId with
          | some pack =>
              IO.println s!"  target solc: {pack.compat.solcVersion}+commit.{pack.compat.solcCommit}"
              IO.println s!"  optimizer runs: {pack.compat.optimizerRuns}"
              IO.println s!"  viaIR: {pack.compat.viaIR}"
              IO.println s!"  evmVersion: {pack.compat.evmVersion}"
          | none => pure ()
      | none => pure ()
      let patchEnabled := patchEnabledFor cfg
      if patchEnabled then
        IO.println s!"Patch pass: enabled (max iterations = {cfg.patchMaxIterations})"
        IO.println s!"Rewrite bundle: {cfg.rewriteBundleId}"
        IO.println s!"Registered proof refs: {cfg.requiredProofRefs.length}"
      IO.println s!"Mapping slot scratch base: {cfg.mappingSlotScratchBase}"
      if !cfg.libs.isEmpty then
        IO.println s!"Linking {cfg.libs.length} external libraries"
        for lib in cfg.libs do
          IO.println s!"  - {lib}"

    let loweredSpec ← lowerMorphoGeneratedSpec
    let selectors ← resolveMorphoGeneratedSelectors
    let ir ← orThrow (compile loweredSpec selectors)
    writeContract cfg.outDir ir cfg.libs (morphoEmitOptions cfg)
    match cfg.abiOutDir with
    | some dir =>
        IO.FS.createDirAll dir
        Compiler.ABI.writeContractABIFile dir loweredSpec
        if cfg.verbose then
          IO.println s!"✓ Wrote {dir}/{ir.name}.abi.json"
    | none => pure ()

    if cfg.verbose then
      IO.println s!"✓ Wrote {cfg.outDir}/{ir.name}.yul"
  catch e =>
    if e.toString == "help" then
      pure ()
    else
      throw e

end Morpho.Compiler.Main
