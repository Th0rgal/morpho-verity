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

namespace abi_decode_bool

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.abi_decode_bool.semantic_eq"
    "abi_decode_bool-canonicalization"
    "abi_decode_bool"

end abi_decode_bool

namespace abi_decode_bytes

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.abi_decode_bytes.semantic_eq"
    "abi_decode_bytes-canonicalization"
    "abi_decode_bytes"

end abi_decode_bytes

namespace abi_decode_struct

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.abi_decode_struct.semantic_eq"
    "abi_decode_struct-canonicalization"
    "abi_decode_struct"

end abi_decode_struct

namespace abi_encode_address

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.abi_encode_address.semantic_eq"
    "abi_encode_address-canonicalization"
    "abi_encode_address"

end abi_encode_address

namespace abi_encode_uint256

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.abi_encode_uint256.semantic_eq"
    "abi_encode_uint256-canonicalization"
    "abi_encode_uint256"

end abi_encode_uint256

namespace array_allocation_size

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.array_allocation_size.semantic_eq"
    "array_allocation_size-canonicalization"
    "array_allocation_size"

end array_allocation_size

namespace checked_div

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.checked_div.semantic_eq"
    "checked_div-canonicalization"
    "checked_div"

end checked_div

namespace checked_mul

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.checked_mul.semantic_eq"
    "checked_mul-canonicalization"
    "checked_mul"

end checked_mul

namespace checked_sub

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.checked_sub.semantic_eq"
    "checked_sub-canonicalization"
    "checked_sub"

end checked_sub

namespace extract_returndata

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.extract_returndata.semantic_eq"
    "extract_returndata-canonicalization"
    "extract_returndata"

end extract_returndata

namespace finalize_allocation

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.finalize_allocation.semantic_eq"
    "finalize_allocation-canonicalization"
    "finalize_allocation"

end finalize_allocation

namespace fun_helpers

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.fun_helpers.semantic_eq"
    "fun-canonicalization"
    "fun"

end fun_helpers

namespace increment_uint256

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.increment_uint256.semantic_eq"
    "increment_uint256-canonicalization"
    "increment_uint256"

end increment_uint256

namespace internal_modifier_onlyowner

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_modifier_onlyowner.semantic_eq"
    "internal_modifier_onlyowner-canonicalization"
    "internal___modifier_onlyOwner"

end internal_modifier_onlyowner

namespace internal_internal_domain_separator

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_domain_separator.semantic_eq"
    "internal_internal_domain_separator-canonicalization"
    "internal_internal_DOMAIN_SEPARATOR"

end internal_internal_domain_separator

namespace internal_internal_accrueinterest

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_accrueinterest.semantic_eq"
    "internal_internal_accrueinterest-canonicalization"
    "internal_internal_accrueInterest"

end internal_internal_accrueinterest

namespace internal_internal_borrow

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_borrow.semantic_eq"
    "internal_internal_borrow-canonicalization"
    "internal_internal_borrow"

end internal_internal_borrow

namespace internal_internal_createmarket

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_createmarket.semantic_eq"
    "internal_internal_createmarket-canonicalization"
    "internal_internal_createMarket"

end internal_internal_createmarket

namespace internal_internal_enableirm

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_enableirm.semantic_eq"
    "internal_internal_enableirm-canonicalization"
    "internal_internal_enableIrm"

end internal_internal_enableirm

namespace internal_internal_enablelltv

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_enablelltv.semantic_eq"
    "internal_internal_enablelltv-canonicalization"
    "internal_internal_enableLltv"

end internal_internal_enablelltv

namespace internal_internal_fee

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_fee.semantic_eq"
    "internal_internal_fee-canonicalization"
    "internal_internal_fee"

end internal_internal_fee

namespace internal_internal_feerecipient

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_feerecipient.semantic_eq"
    "internal_internal_feerecipient-canonicalization"
    "internal_internal_feeRecipient"

end internal_internal_feerecipient

namespace internal_internal_flashloan

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_flashloan.semantic_eq"
    "internal_internal_flashloan-canonicalization"
    "internal_internal_flashLoan"

end internal_internal_flashloan

namespace internal_internal_idtomarketparams

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_idtomarketparams.semantic_eq"
    "internal_internal_idtomarketparams-canonicalization"
    "internal_internal_idToMarketParams"

end internal_internal_idtomarketparams

namespace internal_internal_isauthorized

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_isauthorized.semantic_eq"
    "internal_internal_isauthorized-canonicalization"
    "internal_internal_isAuthorized"

end internal_internal_isauthorized

namespace internal_internal_isirmenabled

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_isirmenabled.semantic_eq"
    "internal_internal_isirmenabled-canonicalization"
    "internal_internal_isIrmEnabled"

end internal_internal_isirmenabled

namespace internal_internal_islltvenabled

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_islltvenabled.semantic_eq"
    "internal_internal_islltvenabled-canonicalization"
    "internal_internal_isLltvEnabled"

end internal_internal_islltvenabled

namespace internal_internal_lastupdate

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_lastupdate.semantic_eq"
    "internal_internal_lastupdate-canonicalization"
    "internal_internal_lastUpdate"

end internal_internal_lastupdate

namespace internal_internal_liquidate

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_liquidate.semantic_eq"
    "internal_internal_liquidate-canonicalization"
    "internal_internal_liquidate"

end internal_internal_liquidate

namespace internal_internal_market

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_market.semantic_eq"
    "internal_internal_market-canonicalization"
    "internal_internal_market"

end internal_internal_market

namespace internal_internal_nonce

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_nonce.semantic_eq"
    "internal_internal_nonce-canonicalization"
    "internal_internal_nonce"

end internal_internal_nonce

namespace internal_internal_owner

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_owner.semantic_eq"
    "internal_internal_owner-canonicalization"
    "internal_internal_owner"

end internal_internal_owner

namespace internal_internal_position

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_position.semantic_eq"
    "internal_internal_position-canonicalization"
    "internal_internal_position"

end internal_internal_position

namespace internal_internal_repay

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_repay.semantic_eq"
    "internal_internal_repay-canonicalization"
    "internal_internal_repay"

end internal_internal_repay

namespace internal_internal_setauthorization

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_setauthorization.semantic_eq"
    "internal_internal_setauthorization-canonicalization"
    "internal_internal_setAuthorization"

end internal_internal_setauthorization

namespace internal_internal_setauthorizationwithsig

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_setauthorizationwithsig.semantic_eq"
    "internal_internal_setauthorizationwithsig-canonicalization"
    "internal_internal_setAuthorizationWithSig"

end internal_internal_setauthorizationwithsig

namespace internal_internal_setfee

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_setfee.semantic_eq"
    "internal_internal_setfee-canonicalization"
    "internal_internal_setFee"

end internal_internal_setfee

namespace internal_internal_setfeerecipient

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_setfeerecipient.semantic_eq"
    "internal_internal_setfeerecipient-canonicalization"
    "internal_internal_setFeeRecipient"

end internal_internal_setfeerecipient

namespace internal_internal_setowner

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_setowner.semantic_eq"
    "internal_internal_setowner-canonicalization"
    "internal_internal_setOwner"

end internal_internal_setowner

namespace internal_internal_supply

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_supply.semantic_eq"
    "internal_internal_supply-canonicalization"
    "internal_internal_supply"

end internal_internal_supply

namespace internal_internal_supplycollateral

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_supplycollateral.semantic_eq"
    "internal_internal_supplycollateral-canonicalization"
    "internal_internal_supplyCollateral"

end internal_internal_supplycollateral

namespace internal_internal_totalborrowassets

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_totalborrowassets.semantic_eq"
    "internal_internal_totalborrowassets-canonicalization"
    "internal_internal_totalBorrowAssets"

end internal_internal_totalborrowassets

namespace internal_internal_totalborrowshares

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_totalborrowshares.semantic_eq"
    "internal_internal_totalborrowshares-canonicalization"
    "internal_internal_totalBorrowShares"

end internal_internal_totalborrowshares

namespace internal_internal_totalsupplyassets

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_totalsupplyassets.semantic_eq"
    "internal_internal_totalsupplyassets-canonicalization"
    "internal_internal_totalSupplyAssets"

end internal_internal_totalsupplyassets

namespace internal_internal_totalsupplyshares

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_totalsupplyshares.semantic_eq"
    "internal_internal_totalsupplyshares-canonicalization"
    "internal_internal_totalSupplyShares"

end internal_internal_totalsupplyshares

namespace internal_internal_withdraw

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_withdraw.semantic_eq"
    "internal_internal_withdraw-canonicalization"
    "internal_internal_withdraw"

end internal_internal_withdraw

namespace internal_internal_withdrawcollateral

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.internal_internal_withdrawcollateral.semantic_eq"
    "internal_internal_withdrawcollateral-canonicalization"
    "internal_internal_withdrawCollateral"

end internal_internal_withdrawcollateral

namespace require_helper_string

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.require_helper_string.semantic_eq"
    "require_helper_string-canonicalization"
    "require_helper_string"

end require_helper_string

namespace update_storage_value

axiom semantic_eq :
  RewriteProofObligation
    "rewrite.update_storage_value.semantic_eq"
    "update_storage_value-canonicalization"
    "update_storage_value"

end update_storage_value

end rewrite

end Morpho.Proofs.YulRewriteProofs
