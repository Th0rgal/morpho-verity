import Compiler.CompilationModel
import Compiler.Selector
import Morpho.Compiler.Spec

namespace Morpho.Compiler.Generated

open Compiler.CompilationModel

/--
Canonical compiler input boundary for Morpho.

Today this is backed by `Spec.morphoSpec` (manual model). Keeping this alias as
the single import point lets us cut over to macro-generated artifacts without
changing all call sites.
-/
def morphoGeneratedSpec : CompilationModel :=
  Morpho.Compiler.Spec.morphoSpec

/--
Canonical selector boundary derived from the generated spec.

This removes the compiler's runtime dependency on manually maintained selector
lists in `Spec.lean`: selectors are computed from function signatures of the
single spec source consumed by compilation.
-/
def morphoGeneratedSelectors : IO (List Nat) :=
  _root_.Compiler.Selector.computeSelectors morphoGeneratedSpec

end Morpho.Compiler.Generated
