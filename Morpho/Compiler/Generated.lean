import Compiler.CompilationModel
import Compiler.Selector
import Morpho.Compiler.MacroSlice

namespace Morpho.Compiler.Generated

open Compiler.CompilationModel

/--
Canonical compiler input boundary for Morpho.

Backed by the macro-generated `verity_contract` artifact so production compile
path no longer depends on manual `Spec.morphoSpec` authoring.
-/
def morphoGeneratedSpec : CompilationModel :=
  { Morpho.Compiler.MacroSlice.MorphoViewSlice.spec with
      name := "Morpho" }

/--
Canonical selector boundary derived from the generated spec.

This removes the compiler's runtime dependency on manually maintained selector
lists in `Spec.lean`: selectors are computed from function signatures of the
single spec source consumed by compilation.
-/
def morphoGeneratedSelectors : IO (List Nat) :=
  _root_.Compiler.Selector.computeSelectors morphoGeneratedSpec

end Morpho.Compiler.Generated
