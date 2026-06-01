import Lake
open Lake DSL

package «morpho-verity» where
  version := v!"0.1.0"

require verity from git
  "https://github.com/Th0rgal/verity.git" @ "540c9f3dfdb3749110666abc619581e528e41bc9"

@[default_target]
lean_lib «Morpho» where
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

lean_exe «morpho-verity-compiler» where
  root := `MorphoCompiler
