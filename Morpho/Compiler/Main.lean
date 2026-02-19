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

  let text ←
    if allLibFunctions.isEmpty then
      pure (_root_.Compiler.Yul.render yulObj)
    else
      orThrow (renderWithLibraries yulObj allLibFunctions)

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
