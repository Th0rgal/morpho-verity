import Morpho.Compiler.Main

namespace Morpho.Compiler.MainTest

private def contains (haystack needle : String) : Bool :=
  let h := haystack.toList
  let n := needle.toList
  if n.isEmpty then true
  else
    let rec go : List Char → Bool
      | [] => false
      | c :: cs =>
        if (c :: cs).take n.length == n then true
        else go cs
    go h

private def expectErrorContains (label : String) (args : List String) (needle : String) : IO Unit := do
  try
    Morpho.Compiler.Main.main args
    throw (IO.userError s!"✗ {label}: expected failure, command succeeded")
  catch e =>
    let msg := e.toString
    if !contains msg needle then
      throw (IO.userError s!"✗ {label}: expected '{needle}', got:\n{msg}")
    IO.println s!"✓ {label}"

private def expectTrue (label : String) (ok : Bool) : IO Unit := do
  if !ok then
    throw (IO.userError s!"✗ {label}")
  IO.println s!"✓ {label}"

private def fileExists (path : String) : IO Bool := do
  try
    let _ ← IO.FS.readFile path
    pure true
  catch _ =>
    pure false

#eval! do
  expectErrorContains "missing --input value" ["--input"] "Missing value for --input"
  expectErrorContains "invalid --input value" ["--input", "ast"] "Invalid value for --input: ast"
  expectErrorContains "missing --link value" ["--link"] "Missing value for --link"
  expectErrorContains "unknown argument still reported" ["--definitely-unknown-flag"] "Unknown argument: --definitely-unknown-flag"

  let modelOutDir := "/tmp/morpho-main-test-model-out"
  let edslOutDir := "/tmp/morpho-main-test-edsl-out"
  IO.FS.createDirAll modelOutDir
  IO.FS.createDirAll edslOutDir

  let hashLib := "compiler/external-libs/MarketParamsHash.yul"
  Morpho.Compiler.Main.main ["--input", "model", "--output", modelOutDir, "--link", hashLib]
  Morpho.Compiler.Main.main ["--input", "edsl", "--output", edslOutDir, "--link", hashLib]

  let modelYulPath := s!"{modelOutDir}/Morpho.yul"
  let edslYulPath := s!"{edslOutDir}/Morpho.yul"
  let modelExists ← fileExists modelYulPath
  let edslExists ← fileExists edslYulPath
  expectTrue "model mode emits Morpho.yul" modelExists
  expectTrue "edsl mode emits Morpho.yul" edslExists

  let modelYul ← IO.FS.readFile modelYulPath
  let edslYul ← IO.FS.readFile edslYulPath
  expectTrue "model and edsl modes emit identical Morpho.yul" (modelYul == edslYul)

end Morpho.Compiler.MainTest
