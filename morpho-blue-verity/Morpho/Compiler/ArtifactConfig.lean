import Compiler.CompilationModel
import Compiler.Selector
import Morpho.Contract

namespace Morpho.Compiler.ArtifactConfig

open Compiler.CompilationModel

private def internalHelperExternalNames : List String :=
  ["_marketParamsId", "_accrueInterest", "_isSenderAuthorized", "_isHealthy", "_isHealthyWithPrice"]

/--
Artifact packaging for the canonical Morpho contract.

`verity_contract Morpho` already produces the real contract spec at
`Morpho.Contract.Morpho.spec`.  This file does not define a second contract; it
only applies the small amount of packaging the standalone compiler CLI needs:

* force the emitted artifact name to `Morpho`,
* keep internal helper functions out of the external ABI/selector surface, and
* make external linked-library dependencies explicit.

The production compile path therefore still has a single source of truth:
`Morpho/Contract.lean`.
-/
def artifactSpec : CompilationModel :=
  -- Canonical boundary: Morpho.Contract.spec is represented by the generated
  -- namespace value `_root_.Morpho.Contract.Morpho.spec`.
  { _root_.Morpho.Contract.Morpho.spec with
      name := "Morpho"
      functions := _root_.Morpho.Contract.Morpho.spec.functions.filter
        (fun fn => fn.isInternal || !internalHelperExternalNames.contains fn.name)
      externals := [] }

/--
Selector list for the compiler CLI, computed from the adapted macro spec.

Selectors are not hand-maintained; they are derived from the same
`Morpho.Contract.Morpho.spec` source used for code generation.
-/
def artifactSelectors : IO (List Nat) :=
  _root_.Compiler.Selector.computeSelectors artifactSpec

end Morpho.Compiler.ArtifactConfig
