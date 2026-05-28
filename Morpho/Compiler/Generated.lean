import Compiler.CompilationModel
import Compiler.Selector
import Morpho.Contract

namespace Morpho.Compiler.Generated

open Compiler.CompilationModel

/--
Canonical compiler input boundary for Morpho.

Backed by the macro-generated `verity_contract` artifact so production compile
path no longer depends on manual `Spec.morphoSpec` authoring.
-/

def morphoGeneratedSpec : CompilationModel :=
  -- Canonical boundary: Morpho.Contract.spec is represented by the generated
  -- namespace value `_root_.Morpho.Contract.Morpho.spec`.
  { _root_.Morpho.Contract.Morpho.spec with
      name := "Morpho"
      externals := [] }

/--
Canonical selector boundary derived from the generated spec.

This removes the compiler's runtime dependency on manually maintained selector
lists in `Spec.lean`: selectors are computed from function signatures of the
single spec source consumed by compilation.
-/
def morphoGeneratedSelectors : IO (List Nat) :=
  _root_.Compiler.Selector.computeSelectors morphoGeneratedSpec

end Morpho.Compiler.Generated
