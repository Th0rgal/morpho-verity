import Lake
open Lake DSL

package «morpho-verity» where
  version := v!"0.1.0"

require verity from git
  "https://github.com/Th0rgal/verity.git" @ "00c18e3a694201cc0dfd8d8f52abaa0bf308887c"

@[default_target]
lean_lib «Morpho» where
  -- Exclude Morpho.Compiler.Spec (legacy hand-written CompilationModel, 1305 lines).
  -- It is superseded by Contract.lean + Generated.lean; no Lean file imports it.
  globs := #[
    .one `Morpho,
    .one `Morpho.Types,
    .one `Morpho.Contract,
    .submodules `Morpho.Libraries,
    .one `Morpho.Compiler.Generated,
    .one `Morpho.Compiler.Main,
    .one `Morpho.Compiler.MainTest
  ]

lean_exe «morpho-verity-compiler» where
  root := `MorphoCompiler
