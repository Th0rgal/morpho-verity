import Lake
open Lake DSL

package «morpho-verity» where
  version := v!"0.1.0"

require verity from git
  "https://github.com/Th0rgal/verity.git" @ "5ef1f7b14a9f79d64bc4573f08b9496cc954375a"

@[default_target]
lean_lib «Morpho» where
  globs := #[.andSubmodules `Morpho]

lean_exe «morpho-verity-compiler» where
  root := `MorphoCompiler
