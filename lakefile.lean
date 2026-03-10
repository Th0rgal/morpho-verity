import Lake
open Lake DSL

package «morpho-verity» where
  version := v!"0.1.0"

require verity from git
  "https://github.com/Th0rgal/verity.git" @ "7b7c9193"

@[default_target]
lean_lib «Morpho» where
  -- Exclude Morpho.Compiler.Spec (legacy hand-written CompilationModel, 1305 lines).
  -- It is superseded by MacroSlice.lean + Generated.lean; no Lean file imports it.
  -- Python CI scripts still read it as plain text for structural correspondence
  -- checks, so the file stays in-tree but is no longer compiled by Lake.
  globs := #[
    .one `Morpho,
    .one `Morpho.Types,
    .one `Morpho.Morpho,
    .submodules `Morpho.Libraries,
    .submodules `Morpho.Specs,
    .submodules `Morpho.Proofs,
    .one `Morpho.Compiler.Generated,
    .one `Morpho.Compiler.MacroSlice,
    .one `Morpho.Compiler.Main,
    .one `Morpho.Compiler.MainTest
  ]

lean_exe «morpho-verity-compiler» where
  root := `MorphoCompiler
