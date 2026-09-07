import Compiler.CompilationModel
import Compiler.Selector
import Midnight.Contract
import Midnight.Compiler.AdminSliceHybrid

namespace Midnight.Compiler.ArtifactConfig

open Compiler.CompilationModel

/--
Artifact packaging for the focused Midnight proof model.

This is deliberately named `MidnightRCF`, not `Midnight`: the generated artifact
is executable code for the liquidation/accounting proof surface in
`Midnight.Contract`, not a complete replacement for `morpho-midnight/src/Midnight.sol`.
-/
def artifactSpec : CompilationModel :=
  { _root_.Midnight.Contract.MidnightRCF.spec with
      name := "MidnightRCF"
      externals := [] }

def artifactSelectors : IO (List Nat) :=
  _root_.Compiler.Selector.computeSelectors artifactSpec

/--
Artifact packaging for the full `Midnight` contract surface.

This is the beginning of the full `IMidnight` parity artifact. It is named
`Midnight` and is emitted under `artifacts/midnight/`, but the implementation
is still partial until the remaining behavioral entrypoints in
`docs/MIDNIGHT_VERITY_PLAN.md` are implemented.
-/
def fullArtifactSpec : Except String CompilationModel := do
  let base : CompilationModel :=
    { _root_.Midnight.Contract.Midnight.spec with
        name := "Midnight"
        externals := [] }
  Midnight.Compiler.AdminSliceHybrid.assemble base

def fullArtifactSelectors (spec : CompilationModel) : IO (List Nat) :=
  _root_.Compiler.Selector.computeSelectors spec

end Midnight.Compiler.ArtifactConfig
