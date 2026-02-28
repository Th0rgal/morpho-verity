import Lake
open Lake DSL

package «morpho-verity» where
  version := v!"0.1.0"

require verity from git
  "https://github.com/Th0rgal/verity.git" @ "3e8c52b"

@[default_target]
lean_lib «Morpho» where
  globs := #[.andSubmodules `Morpho]

lean_exe «morpho-verity-compiler» where
  root := `MorphoCompiler
