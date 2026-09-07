import Compiler.CompilationModel
import Compiler.Selector
import Midnight.Contract
import Midnight.Generated.FullModel

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

/-- Complete model generated exclusively from the pinned Solidity AST and layout.
The handwritten contract above is used only by the separate focused proof artifact.
-/
def fullArtifactSpec : CompilationModel :=
  _root_.Midnight.Generated.Full.spec

/-- Source selectors are authoritative: the pinned backend represents some narrow
return words using uint256, which must not rewrite the Solidity ABI metadata. -/
def fullArtifactSelectors : IO (List Nat) :=
  pure _root_.Midnight.Generated.Full.selectors

end Midnight.Compiler.ArtifactConfig
