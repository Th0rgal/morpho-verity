import Lake
open Lake DSL

package «morpho-verity» where
  version := v!"0.1.0"

require verity from git
  "https://github.com/Th0rgal/verity.git" @ "760e9feb7161c6a4f26f8bad4bbcfb3950ff02ce"

@[default_target]
lean_lib «Morpho» where
  srcDir := "morpho-blue-verity"
  -- Keep the Morpho library explicit so no hand-written compiler model can
  -- become a second source of truth by glob expansion.
  globs := #[
    .one `Morpho,
    .one `Morpho.Contract,
    .submodules `Morpho.Libraries,
    .one `Morpho.Compiler.ArtifactConfig,
    .one `Morpho.Compiler.Main,
    .one `Morpho.Compiler.MainTest,
    .one `Morpho.Proofs,
    .submodules `Morpho.Proofs
  ]

lean_lib «Midnight» where
  srcDir := "morpho-midnight-verity"
  globs := #[
    .one `Midnight,
    .one `Midnight.Contract,
    .one `Midnight.Compiler.ArtifactConfig,
    .one `Midnight.Compiler.Main,
    .one `Midnight.Proofs,
    .submodules `Midnight.Proofs
  ]

lean_exe «morpho-verity-compiler» where
  srcDir := "morpho-blue-verity"
  root := `MorphoCompiler

lean_exe «midnight-verity-compiler» where
  srcDir := "morpho-midnight-verity"
  root := `MidnightCompiler
