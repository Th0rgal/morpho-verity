import Compiler.CompilationModel
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

end Morpho.Compiler.Generated
