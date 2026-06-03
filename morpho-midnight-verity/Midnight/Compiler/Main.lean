import Compiler.ABI
import Compiler.Codegen
import Compiler.CompilationModel
import Compiler.Yul.PrettyPrint
import Midnight.Compiler.ArtifactConfig

namespace Midnight.Compiler.Main

open _root_.Compiler
open _root_.Compiler.CompilationModel

private structure CLIArgs where
  outDir : String := "artifacts/midnight"
  abiOutDir : Option String := none
  artifact : String := "focused"
  verbose : Bool := false

private def orThrow {α : Type} (r : Except String α) : IO α :=
  match r with
  | .ok v => pure v
  | .error e => throw (IO.userError e)

private def parseArgs (args : List String) : IO CLIArgs :=
  let rec go : List String → CLIArgs → IO CLIArgs
    | [], cfg => pure cfg
    | "--output" :: dir :: rest, cfg => go rest { cfg with outDir := dir }
    | "-o" :: dir :: rest, cfg => go rest { cfg with outDir := dir }
    | "--abi-output" :: dir :: rest, cfg => go rest { cfg with abiOutDir := some dir }
    | "--artifact" :: artifact :: rest, cfg =>
        if artifact == "focused" || artifact == "full" then
          go rest { cfg with artifact := artifact }
        else
          throw (IO.userError s!"Invalid --artifact value: {artifact} (expected focused or full)")
    | "--verbose" :: rest, cfg => go rest { cfg with verbose := true }
    | "-v" :: rest, cfg => go rest { cfg with verbose := true }
    | ["--output"], _ | ["-o"], _ =>
        throw (IO.userError "Missing value for --output")
    | ["--abi-output"], _ =>
        throw (IO.userError "Missing value for --abi-output")
    | ["--artifact"], _ =>
        throw (IO.userError "Missing value for --artifact")
    | "--help" :: _, _
    | "-h" :: _, _ => do
        IO.println "Usage: midnight-verity-compiler [options]"
        IO.println ""
        IO.println "Options:"
        IO.println "  --output <dir>, -o <dir>    Output directory (default: artifacts/midnight)"
        IO.println "  --abi-output <dir>          Output ABI JSON artifact (<Contract>.abi.json)"
        IO.println "  --artifact <focused|full>   Artifact surface (default: focused)"
        IO.println "  --verbose, -v               Verbose logs"
        IO.println "  --help, -h                  Show this help"
        throw (IO.userError "help")
    | x :: _, _ =>
        throw (IO.userError s!"Unknown argument: {x}")
  go args {}

private def writeContract (outDir : String) (contract : IRContract) : IO Unit := do
  let yulObj := _root_.Compiler.emitYul contract
  let text := _root_.Compiler.Yul.render yulObj
  IO.FS.createDirAll outDir
  IO.FS.writeFile s!"{outDir}/{contract.name}.yul" text

def main (args : List String) : IO Unit := do
  try
    let cfg ← parseArgs args
    if cfg.verbose then
      IO.println s!"Compiling Midnight {cfg.artifact} artifact to {cfg.outDir}"
      match cfg.abiOutDir with
      | some dir => IO.println s!"ABI output directory: {dir}"
      | none => pure ()

    let (spec, selectors) ←
      if cfg.artifact == "full" then
        let selectors ← Midnight.Compiler.ArtifactConfig.fullArtifactSelectors
        pure (Midnight.Compiler.ArtifactConfig.fullArtifactSpec, selectors)
      else
        let selectors ← Midnight.Compiler.ArtifactConfig.artifactSelectors
        pure (Midnight.Compiler.ArtifactConfig.artifactSpec, selectors)
    let ir ← orThrow (compile spec selectors)
    writeContract cfg.outDir ir
    match cfg.abiOutDir with
    | some dir =>
        IO.FS.createDirAll dir
        Compiler.ABI.writeContractABIFile dir spec
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

end Midnight.Compiler.Main
