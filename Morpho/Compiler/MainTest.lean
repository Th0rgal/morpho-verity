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
  expectErrorContains "input mode flag removed" ["--input", "edsl"] "Unknown argument: --input"
  expectErrorContains "missing --link value" ["--link"] "Missing value for --link"
  expectErrorContains "missing --abi-output value" ["--abi-output"] "Missing value for --abi-output"
  expectErrorContains "unknown argument still reported" ["--definitely-unknown-flag"] "Unknown argument: --definitely-unknown-flag"

  let nonce ← IO.monoMsNow
  let edslOutDir := s!"/tmp/morpho-main-test-{nonce}-edsl-out"
  let edslAbiDir := s!"/tmp/morpho-main-test-{nonce}-edsl-abi"
  IO.FS.createDirAll edslOutDir
  IO.FS.createDirAll edslAbiDir

  let hashLib := "artifacts/inputs/MarketParamsHash.yul"
  Morpho.Compiler.Main.main
    ["--output", edslOutDir, "--abi-output", edslAbiDir, "--link", hashLib]

  let edslYulPath := s!"{edslOutDir}/Morpho.yul"
  let edslExists ← fileExists edslYulPath
  expectTrue "edsl mode emits Morpho.yul" edslExists
  let edslAbiPath := s!"{edslAbiDir}/Morpho.abi.json"
  let edslAbiExists ← fileExists edslAbiPath
  expectTrue "edsl mode emits Morpho.abi.json" edslAbiExists

end Morpho.Compiler.MainTest
