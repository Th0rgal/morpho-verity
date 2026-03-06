/-!
# Yul Rewrite Proof Skeletons

This module declares the proof placeholders referenced by
`config/yul-rewrite-proof-obligations.json`.

Each declaration names the semantic-preservation result that must eventually
justify a concrete Yul rewrite pass on the path from raw Verity-emitted Yul to
exact Solidity-emitted Yul. They are axioms for now so the backlog is
machine-checkable without pretending the proofs are already discharged.
-/

namespace Morpho.Proofs.YulRewriteProofs

/-- Placeholder proposition used to name a Yul rewrite proof obligation.
    The current declarations are tracking skeletons only; real theorem
    statements can refine this proposition once the rewrite semantics are
    formalized in Lean. -/
def RewriteProofObligation (_proofRef _rewritePass _family : String) : Prop := True

namespace rewrite

namespace rename_only

axiom alpha_equiv :
  RewriteProofObligation
    "rewrite.rename_only.alpha_equiv"
    "rename-normalization"
    "renameOnly"

end rename_only

namespace rename_ambiguous

axiom disambiguate :
  RewriteProofObligation
    "rewrite.rename_ambiguous.disambiguate"
    "rename-family-disambiguation"
    "renameAmbiguous"

end rename_ambiguous

namespace checked_add

axiom width_alignment :
  RewriteProofObligation
    "rewrite.checked_add.width_alignment"
    "checked-arith-width-alignment"
    "checked_add"

end checked_add

namespace copy_literal_to_memory

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.copy_literal_to_memory.semantic_eq"
    "literal-memory-canonicalization"
    "copy_literal_to_memory"

end copy_literal_to_memory

namespace abi_decode_address

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.abi_decode_address.semantic_eq"
    "abi-decode-helper-canonicalization"
    "abi_decode_address"

end abi_decode_address

namespace abi_encode_struct

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.abi_encode_struct.semantic_eq"
    "abi-encode-struct-canonicalization"
    "abi_encode_struct"

end abi_encode_struct

namespace mapping_slot

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.mapping_slot.semantic_eq"
    "mapping-slot-helper-canonicalization"
    "mappingSlot"

end mapping_slot

namespace market_params_hash

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.market_params_hash.semantic_eq"
    "market-params-hash-canonicalization"
    "keccakMarketParams"

end market_params_hash

end rewrite

end Morpho.Proofs.YulRewriteProofs
