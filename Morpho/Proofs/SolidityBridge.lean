import Morpho.Proofs.Invariants

namespace Morpho.Proofs.SolidityBridge

open Verity
open Morpho.Types
open Morpho.Specs.Invariants
open Morpho.Proofs.Invariants

/--
A formal bridge: once Solidity Yul semantics are proven equivalent to the
corresponding Verity functions, existing Verity invariant proofs transfer
immediately to Solidity.

Each theorem below isolates one Morpho operation and states the exact
equivalence hypothesis needed for proof transfer.
-/

abbrev SupplySem :=
  MorphoState → Id → Uint256 → Uint256 → Address → Option (Uint256 × Uint256 × MorphoState)

abbrev WithdrawSem :=
  MorphoState → Id → Uint256 → Uint256 → Address → Address → Option (Uint256 × Uint256 × MorphoState)

abbrev BorrowSem :=
  MorphoState → Id → Uint256 → Uint256 → Address → Address → Uint256 → Uint256 →
    Option (Uint256 × Uint256 × MorphoState)

abbrev RepaySem :=
  MorphoState → Id → Uint256 → Uint256 → Address → Option (Uint256 × Uint256 × MorphoState)

abbrev SupplyCollateralSem :=
  MorphoState → Id → Uint256 → Address → Option MorphoState

abbrev WithdrawCollateralSem :=
  MorphoState → Id → Uint256 → Address → Address → Uint256 → Uint256 → Option MorphoState

abbrev LiquidateSem :=
  MorphoState → Id → Address → Uint256 → Uint256 → Uint256 → Uint256 →
    Option (Uint256 × Uint256 × MorphoState)

abbrev AccrueInterestSem :=
  MorphoState → Id → Uint256 → Bool → MorphoState

def supplySemEq (soliditySupply : SupplySem) : Prop :=
  ∀ s id assets shares onBehalf,
    soliditySupply s id assets shares onBehalf = Morpho.supply s id assets shares onBehalf

def withdrawSemEq (solidityWithdraw : WithdrawSem) : Prop :=
  ∀ s id assets shares onBehalf receiver,
    solidityWithdraw s id assets shares onBehalf receiver =
      Morpho.withdraw s id assets shares onBehalf receiver

def borrowSemEq (solidityBorrow : BorrowSem) : Prop :=
  ∀ s id assets shares onBehalf receiver collateralPrice lltv,
    solidityBorrow s id assets shares onBehalf receiver collateralPrice lltv =
      Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv

def repaySemEq (solidityRepay : RepaySem) : Prop :=
  ∀ s id assets shares onBehalf,
    solidityRepay s id assets shares onBehalf = Morpho.repay s id assets shares onBehalf

def supplyCollateralSemEq (soliditySupplyCollateral : SupplyCollateralSem) : Prop :=
  ∀ s id assets onBehalf,
    soliditySupplyCollateral s id assets onBehalf = Morpho.supplyCollateral s id assets onBehalf

def withdrawCollateralSemEq (solidityWithdrawCollateral : WithdrawCollateralSem) : Prop :=
  ∀ s id assets onBehalf receiver collateralPrice lltv,
    solidityWithdrawCollateral s id assets onBehalf receiver collateralPrice lltv =
      Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv

def liquidateSemEq (solidityLiquidate : LiquidateSem) : Prop :=
  ∀ s id borrower seizedAssets repaidShares collateralPrice lltv,
    solidityLiquidate s id borrower seizedAssets repaidShares collateralPrice lltv =
      Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv

def accrueInterestSemEq (solidityAccrue : AccrueInterestSem) : Prop :=
  ∀ s id borrowRate hasIrm,
    solidityAccrue s id borrowRate hasIrm = Morpho.accrueInterest s id borrowRate hasIrm

theorem solidity_supply_preserves_borrowLeSupply
    (soliditySupply : SupplySem)
    (h_eq : supplySemEq soliditySupply)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    (a sh : Uint256) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : soliditySupply s id assets shares onBehalf = some (a, sh, s'))
    (h_no_overflow : (s.market id).totalSupplyAssets.val + a.val < Verity.Core.Uint256.modulus) :
    borrowLeSupply s' id := by
  have h_ok_morpho : Morpho.supply s id assets shares onBehalf = some (a, sh, s') := by
    simpa [supplySemEq] using (h_eq s id assets shares onBehalf).symm.trans h_ok
  exact supply_preserves_borrowLeSupply s id assets shares onBehalf h_solvent h_ok_morpho h_no_overflow

theorem solidity_withdraw_preserves_borrowLeSupply
    (solidityWithdraw : WithdrawSem)
    (h_eq : withdrawSemEq solidityWithdraw)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver : Address)
    (a sh : Uint256) (s' : MorphoState)
    (h_ok : solidityWithdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    borrowLeSupply s' id := by
  have h_ok_morpho : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s') := by
    simpa [withdrawSemEq] using (h_eq s id assets shares onBehalf receiver).symm.trans h_ok
  exact withdraw_preserves_borrowLeSupply s id assets shares onBehalf receiver h_ok_morpho

theorem solidity_borrow_preserves_borrowLeSupply
    (solidityBorrow : BorrowSem)
    (h_eq : borrowSemEq solidityBorrow)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256)
    (a sh : Uint256) (s' : MorphoState)
    (h_ok : solidityBorrow s id assets shares onBehalf receiver collateralPrice lltv =
      some (a, sh, s')) :
    borrowLeSupply s' id := by
  have h_ok_morpho : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv =
      some (a, sh, s') := by
    simpa [borrowSemEq] using
      (h_eq s id assets shares onBehalf receiver collateralPrice lltv).symm.trans h_ok
  exact borrow_preserves_borrowLeSupply s id assets shares onBehalf receiver collateralPrice lltv h_ok_morpho

theorem solidity_repay_preserves_borrowLeSupply
    (solidityRepay : RepaySem)
    (h_eq : repaySemEq solidityRepay)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    (a sh : Uint256) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : solidityRepay s id assets shares onBehalf = some (a, sh, s')) :
    borrowLeSupply s' id := by
  have h_ok_morpho : Morpho.repay s id assets shares onBehalf = some (a, sh, s') := by
    simpa [repaySemEq] using (h_eq s id assets shares onBehalf).symm.trans h_ok
  exact repay_preserves_borrowLeSupply s id assets shares onBehalf h_solvent h_ok_morpho

theorem solidity_supplyCollateral_preserves_borrowLeSupply
    (soliditySupplyCollateral : SupplyCollateralSem)
    (h_eq : supplyCollateralSemEq soliditySupplyCollateral)
    (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf : Address)
    (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : soliditySupplyCollateral s id assets onBehalf = some s') :
    borrowLeSupply s' id := by
  have h_ok_morpho : Morpho.supplyCollateral s id assets onBehalf = some s' := by
    simpa [supplyCollateralSemEq] using (h_eq s id assets onBehalf).symm.trans h_ok
  exact supplyCollateral_preserves_borrowLeSupply s id assets onBehalf h_solvent h_ok_morpho

theorem solidity_withdrawCollateral_preserves_borrowLeSupply
    (solidityWithdrawCollateral : WithdrawCollateralSem)
    (h_eq : withdrawCollateralSemEq solidityWithdrawCollateral)
    (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltv : Uint256) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok :
      solidityWithdrawCollateral s id assets onBehalf receiver collateralPrice lltv = some s') :
    borrowLeSupply s' id := by
  have h_ok_morpho : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv =
      some s' := by
    simpa [withdrawCollateralSemEq] using
      (h_eq s id assets onBehalf receiver collateralPrice lltv).symm.trans h_ok
  exact withdrawCollateral_preserves_borrowLeSupply
    s id assets onBehalf receiver collateralPrice lltv h_solvent h_ok_morpho

theorem solidity_liquidate_preserves_borrowLeSupply
    (solidityLiquidate : LiquidateSem)
    (h_eq : liquidateSemEq solidityLiquidate)
    (s : MorphoState) (id : Id) (borrower : Address)
    (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (seized repaid : Uint256) (s' : MorphoState)
    (h_solvent : borrowLeSupply s id)
    (h_ok : solidityLiquidate s id borrower seizedAssets repaidShares collateralPrice lltv =
      some (seized, repaid, s')) :
    borrowLeSupply s' id := by
  have h_ok_morpho : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv =
      some (seized, repaid, s') := by
    simpa [liquidateSemEq] using
      (h_eq s id borrower seizedAssets repaidShares collateralPrice lltv).symm.trans h_ok
  exact liquidate_preserves_borrowLeSupply
    s id borrower seizedAssets repaidShares collateralPrice lltv h_solvent h_ok_morpho

theorem solidity_accrueInterest_preserves_borrowLeSupply
    (solidityAccrue : AccrueInterestSem)
    (h_eq : accrueInterestSemEq solidityAccrue)
    (s : MorphoState) (id : Id) (borrowRate : Uint256) (hasIrm : Bool)
    (h_solvent : borrowLeSupply s id)
    (h_no_overflow : (s.market id).totalSupplyAssets.val +
      (Morpho.Libraries.MathLib.wMulDown (s.market id).totalBorrowAssets
        (Morpho.Libraries.MathLib.wTaylorCompounded borrowRate
          (Morpho.u256 (s.blockTimestamp.val - (s.market id).lastUpdate.val)))).val
      < Verity.Core.Uint256.modulus) :
    borrowLeSupply (solidityAccrue s id borrowRate hasIrm) id := by
  have h_eq_morpho : solidityAccrue s id borrowRate hasIrm = Morpho.accrueInterest s id borrowRate hasIrm :=
    h_eq s id borrowRate hasIrm
  rw [h_eq_morpho]
  exact accrueInterest_preserves_borrowLeSupply s id borrowRate hasIrm h_solvent h_no_overflow

theorem solidity_accrueInterest_preserves_alwaysCollateralized
    (solidityAccrue : AccrueInterestSem)
    (h_eq : accrueInterestSemEq solidityAccrue)
    (s : MorphoState) (id : Id) (borrowRate : Uint256) (hasIrm : Bool) (user : Address)
    (h_collat : alwaysCollateralized s id user) :
    alwaysCollateralized (solidityAccrue s id borrowRate hasIrm) id user := by
  have h_eq_morpho : solidityAccrue s id borrowRate hasIrm = Morpho.accrueInterest s id borrowRate hasIrm :=
    h_eq s id borrowRate hasIrm
  rw [h_eq_morpho]
  exact accrueInterest_preserves_alwaysCollateralized s id borrowRate hasIrm user h_collat

theorem solidity_supply_preserves_alwaysCollateralized
    (soliditySupply : SupplySem)
    (h_eq : supplySemEq soliditySupply)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf user : Address)
    (a sh : Uint256) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : soliditySupply s id assets shares onBehalf = some (a, sh, s')) :
    alwaysCollateralized s' id user := by
  have h_ok_morpho : Morpho.supply s id assets shares onBehalf = some (a, sh, s') := by
    simpa [supplySemEq] using (h_eq s id assets shares onBehalf).symm.trans h_ok
  exact supply_preserves_alwaysCollateralized s id assets shares onBehalf user h_collat h_ok_morpho

theorem solidity_withdraw_preserves_alwaysCollateralized
    (solidityWithdraw : WithdrawSem)
    (h_eq : withdrawSemEq solidityWithdraw)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver user : Address)
    (a sh : Uint256) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : solidityWithdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    alwaysCollateralized s' id user := by
  have h_ok_morpho : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s') := by
    simpa [withdrawSemEq] using (h_eq s id assets shares onBehalf receiver).symm.trans h_ok
  exact withdraw_preserves_alwaysCollateralized
    s id assets shares onBehalf receiver user h_collat h_ok_morpho

theorem solidity_repay_preserves_alwaysCollateralized
    (solidityRepay : RepaySem)
    (h_eq : repaySemEq solidityRepay)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf user : Address)
    (a sh : Uint256) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : solidityRepay s id assets shares onBehalf = some (a, sh, s')) :
    alwaysCollateralized s' id user := by
  have h_ok_morpho : Morpho.repay s id assets shares onBehalf = some (a, sh, s') := by
    simpa [repaySemEq] using (h_eq s id assets shares onBehalf).symm.trans h_ok
  exact repay_preserves_alwaysCollateralized s id assets shares onBehalf user h_collat h_ok_morpho

theorem solidity_supplyCollateral_preserves_alwaysCollateralized
    (soliditySupplyCollateral : SupplyCollateralSem)
    (h_eq : supplyCollateralSemEq soliditySupplyCollateral)
    (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf user : Address)
    (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : soliditySupplyCollateral s id assets onBehalf = some s')
    (h_no_overflow : user = onBehalf →
      (s.position id user).collateral.val + assets.val < Verity.Core.Uint256.modulus) :
    alwaysCollateralized s' id user := by
  have h_ok_morpho : Morpho.supplyCollateral s id assets onBehalf = some s' := by
    simpa [supplyCollateralSemEq] using (h_eq s id assets onBehalf).symm.trans h_ok
  exact supplyCollateral_preserves_alwaysCollateralized
    s id assets onBehalf user h_collat h_ok_morpho h_no_overflow

theorem solidity_borrow_preserves_alwaysCollateralized
    (solidityBorrow : BorrowSem)
    (h_eq : borrowSemEq solidityBorrow)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver user : Address)
    (collateralPrice lltv : Uint256)
    (a sh : Uint256) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok : solidityBorrow s id assets shares onBehalf receiver collateralPrice lltv =
      some (a, sh, s'))
    (h_borrowed_pos : user = onBehalf →
      (Morpho.Libraries.SharesMathLib.toAssetsUp
        (s'.position id user).borrowShares
        (s'.market id).totalBorrowAssets
        (s'.market id).totalBorrowShares).val > 0) :
    alwaysCollateralized s' id user := by
  have h_ok_morpho : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltv =
      some (a, sh, s') := by
    simpa [borrowSemEq] using
      (h_eq s id assets shares onBehalf receiver collateralPrice lltv).symm.trans h_ok
  exact borrow_preserves_alwaysCollateralized
    s id assets shares onBehalf receiver user collateralPrice lltv h_collat h_ok_morpho h_borrowed_pos

theorem solidity_liquidate_preserves_alwaysCollateralized
    (solidityLiquidate : LiquidateSem)
    (h_eq : liquidateSemEq solidityLiquidate)
    (s : MorphoState) (id : Id) (borrower : Address)
    (seizedAssets repaidShares collateralPrice lltv : Uint256)
    (seized repaid : Uint256) (s' : MorphoState)
    (h_ok : solidityLiquidate s id borrower seizedAssets repaidShares collateralPrice lltv =
      some (seized, repaid, s')) :
    alwaysCollateralized s' id borrower := by
  have h_ok_morpho : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltv =
      some (seized, repaid, s') := by
    simpa [liquidateSemEq] using
      (h_eq s id borrower seizedAssets repaidShares collateralPrice lltv).symm.trans h_ok
  exact liquidate_preserves_alwaysCollateralized
    s id borrower seizedAssets repaidShares collateralPrice lltv h_ok_morpho

theorem solidity_withdrawCollateral_preserves_alwaysCollateralized
    (solidityWithdrawCollateral : WithdrawCollateralSem)
    (h_eq : withdrawCollateralSemEq solidityWithdrawCollateral)
    (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf receiver user : Address)
    (collateralPrice lltv : Uint256) (s' : MorphoState)
    (h_collat : alwaysCollateralized s id user)
    (h_ok :
      solidityWithdrawCollateral s id assets onBehalf receiver collateralPrice lltv = some s')
    (h_borrowed_pos : user = onBehalf →
      (Morpho.Libraries.SharesMathLib.toAssetsUp
        (s'.position id user).borrowShares
        (s'.market id).totalBorrowAssets
        (s'.market id).totalBorrowShares).val > 0) :
    alwaysCollateralized s' id user := by
  have h_ok_morpho : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltv =
      some s' := by
    simpa [withdrawCollateralSemEq] using
      (h_eq s id assets onBehalf receiver collateralPrice lltv).symm.trans h_ok
  exact withdrawCollateral_preserves_alwaysCollateralized
    s id assets onBehalf receiver user collateralPrice lltv h_collat h_ok_morpho h_borrowed_pos

theorem solidity_withdrawCollateral_preserves_irmMonotone
    (solidityWithdrawCollateral : WithdrawCollateralSem)
    (h_eq : withdrawCollateralSemEq solidityWithdrawCollateral)
    (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltvParam : Uint256) (irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok :
      solidityWithdrawCollateral s id assets onBehalf receiver collateralPrice lltvParam = some s') :
    s'.isIrmEnabled irm := by
  have h_ok_morpho : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltvParam =
      some s' := by
    simpa [withdrawCollateralSemEq] using
      (h_eq s id assets onBehalf receiver collateralPrice lltvParam).symm.trans h_ok
  exact withdrawCollateral_preserves_irmMonotone
    s id assets onBehalf receiver collateralPrice lltvParam irm h_enabled h_ok_morpho

theorem solidity_withdrawCollateral_preserves_lltvMonotone
    (solidityWithdrawCollateral : WithdrawCollateralSem)
    (h_eq : withdrawCollateralSemEq solidityWithdrawCollateral)
    (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltvParam lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok :
      solidityWithdrawCollateral s id assets onBehalf receiver collateralPrice lltvParam = some s') :
    s'.isLltvEnabled lltv := by
  have h_ok_morpho : Morpho.withdrawCollateral s id assets onBehalf receiver collateralPrice lltvParam =
      some s' := by
    simpa [withdrawCollateralSemEq] using
      (h_eq s id assets onBehalf receiver collateralPrice lltvParam).symm.trans h_ok
  exact withdrawCollateral_preserves_lltvMonotone
    s id assets onBehalf receiver collateralPrice lltvParam lltv h_enabled h_ok_morpho

theorem solidity_supply_preserves_irmMonotone
    (soliditySupply : SupplySem)
    (h_eq : supplySemEq soliditySupply)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    (irm : Address) (a sh : Uint256) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : soliditySupply s id assets shares onBehalf = some (a, sh, s')) :
    s'.isIrmEnabled irm := by
  have h_ok_morpho : Morpho.supply s id assets shares onBehalf = some (a, sh, s') := by
    simpa [supplySemEq] using (h_eq s id assets shares onBehalf).symm.trans h_ok
  exact supply_preserves_irmMonotone s id assets shares onBehalf irm h_enabled h_ok_morpho

theorem solidity_withdraw_preserves_irmMonotone
    (solidityWithdraw : WithdrawSem)
    (h_eq : withdrawSemEq solidityWithdraw)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver : Address)
    (irm : Address) (a sh : Uint256) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : solidityWithdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    s'.isIrmEnabled irm := by
  have h_ok_morpho : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s') := by
    simpa [withdrawSemEq] using (h_eq s id assets shares onBehalf receiver).symm.trans h_ok
  exact withdraw_preserves_irmMonotone
    s id assets shares onBehalf receiver irm h_enabled h_ok_morpho

theorem solidity_borrow_preserves_irmMonotone
    (solidityBorrow : BorrowSem)
    (h_eq : borrowSemEq solidityBorrow)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltvParam : Uint256) (irm : Address)
    (a sh : Uint256) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : solidityBorrow s id assets shares onBehalf receiver collateralPrice lltvParam =
      some (a, sh, s')) :
    s'.isIrmEnabled irm := by
  have h_ok_morpho : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltvParam =
      some (a, sh, s') := by
    simpa [borrowSemEq] using
      (h_eq s id assets shares onBehalf receiver collateralPrice lltvParam).symm.trans h_ok
  exact borrow_preserves_irmMonotone
    s id assets shares onBehalf receiver collateralPrice lltvParam irm h_enabled h_ok_morpho

theorem solidity_repay_preserves_irmMonotone
    (solidityRepay : RepaySem)
    (h_eq : repaySemEq solidityRepay)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    (irm : Address) (a sh : Uint256) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : solidityRepay s id assets shares onBehalf = some (a, sh, s')) :
    s'.isIrmEnabled irm := by
  have h_ok_morpho : Morpho.repay s id assets shares onBehalf = some (a, sh, s') := by
    simpa [repaySemEq] using (h_eq s id assets shares onBehalf).symm.trans h_ok
  exact repay_preserves_irmMonotone s id assets shares onBehalf irm h_enabled h_ok_morpho

theorem solidity_supplyCollateral_preserves_irmMonotone
    (soliditySupplyCollateral : SupplyCollateralSem)
    (h_eq : supplyCollateralSemEq soliditySupplyCollateral)
    (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf : Address)
    (irm : Address) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : soliditySupplyCollateral s id assets onBehalf = some s') :
    s'.isIrmEnabled irm := by
  have h_ok_morpho : Morpho.supplyCollateral s id assets onBehalf = some s' := by
    simpa [supplyCollateralSemEq] using (h_eq s id assets onBehalf).symm.trans h_ok
  exact supplyCollateral_preserves_irmMonotone s id assets onBehalf irm h_enabled h_ok_morpho

theorem solidity_liquidate_preserves_irmMonotone
    (solidityLiquidate : LiquidateSem)
    (h_eq : liquidateSemEq solidityLiquidate)
    (s : MorphoState) (id : Id) (borrower : Address)
    (seizedAssets repaidShares collateralPrice lltvParam : Uint256)
    (irm : Address) (seized repaid : Uint256) (s' : MorphoState)
    (h_enabled : s.isIrmEnabled irm)
    (h_ok : solidityLiquidate s id borrower seizedAssets repaidShares collateralPrice lltvParam =
      some (seized, repaid, s')) :
    s'.isIrmEnabled irm := by
  have h_ok_morpho : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltvParam =
      some (seized, repaid, s') := by
    simpa [liquidateSemEq] using
      (h_eq s id borrower seizedAssets repaidShares collateralPrice lltvParam).symm.trans h_ok
  exact liquidate_preserves_irmMonotone
    s id borrower seizedAssets repaidShares collateralPrice lltvParam irm h_enabled h_ok_morpho

theorem solidity_supply_preserves_lltvMonotone
    (soliditySupply : SupplySem)
    (h_eq : supplySemEq soliditySupply)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    (lltv : Uint256) (a sh : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : soliditySupply s id assets shares onBehalf = some (a, sh, s')) :
    s'.isLltvEnabled lltv := by
  have h_ok_morpho : Morpho.supply s id assets shares onBehalf = some (a, sh, s') := by
    simpa [supplySemEq] using (h_eq s id assets shares onBehalf).symm.trans h_ok
  exact supply_preserves_lltvMonotone s id assets shares onBehalf lltv h_enabled h_ok_morpho

theorem solidity_withdraw_preserves_lltvMonotone
    (solidityWithdraw : WithdrawSem)
    (h_eq : withdrawSemEq solidityWithdraw)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver : Address)
    (lltv : Uint256) (a sh : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : solidityWithdraw s id assets shares onBehalf receiver = some (a, sh, s')) :
    s'.isLltvEnabled lltv := by
  have h_ok_morpho : Morpho.withdraw s id assets shares onBehalf receiver = some (a, sh, s') := by
    simpa [withdrawSemEq] using (h_eq s id assets shares onBehalf receiver).symm.trans h_ok
  exact withdraw_preserves_lltvMonotone
    s id assets shares onBehalf receiver lltv h_enabled h_ok_morpho

theorem solidity_borrow_preserves_lltvMonotone
    (solidityBorrow : BorrowSem)
    (h_eq : borrowSemEq solidityBorrow)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf receiver : Address)
    (collateralPrice lltvParam lltv : Uint256) (a sh : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : solidityBorrow s id assets shares onBehalf receiver collateralPrice lltvParam =
      some (a, sh, s')) :
    s'.isLltvEnabled lltv := by
  have h_ok_morpho : Morpho.borrow s id assets shares onBehalf receiver collateralPrice lltvParam =
      some (a, sh, s') := by
    simpa [borrowSemEq] using
      (h_eq s id assets shares onBehalf receiver collateralPrice lltvParam).symm.trans h_ok
  exact borrow_preserves_lltvMonotone
    s id assets shares onBehalf receiver collateralPrice lltvParam lltv h_enabled h_ok_morpho

theorem solidity_repay_preserves_lltvMonotone
    (solidityRepay : RepaySem)
    (h_eq : repaySemEq solidityRepay)
    (s : MorphoState) (id : Id) (assets shares : Uint256) (onBehalf : Address)
    (lltv : Uint256) (a sh : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : solidityRepay s id assets shares onBehalf = some (a, sh, s')) :
    s'.isLltvEnabled lltv := by
  have h_ok_morpho : Morpho.repay s id assets shares onBehalf = some (a, sh, s') := by
    simpa [repaySemEq] using (h_eq s id assets shares onBehalf).symm.trans h_ok
  exact repay_preserves_lltvMonotone s id assets shares onBehalf lltv h_enabled h_ok_morpho

theorem solidity_supplyCollateral_preserves_lltvMonotone
    (soliditySupplyCollateral : SupplyCollateralSem)
    (h_eq : supplyCollateralSemEq soliditySupplyCollateral)
    (s : MorphoState) (id : Id) (assets : Uint256) (onBehalf : Address)
    (lltv : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : soliditySupplyCollateral s id assets onBehalf = some s') :
    s'.isLltvEnabled lltv := by
  have h_ok_morpho : Morpho.supplyCollateral s id assets onBehalf = some s' := by
    simpa [supplyCollateralSemEq] using (h_eq s id assets onBehalf).symm.trans h_ok
  exact supplyCollateral_preserves_lltvMonotone s id assets onBehalf lltv h_enabled h_ok_morpho

theorem solidity_liquidate_preserves_lltvMonotone
    (solidityLiquidate : LiquidateSem)
    (h_eq : liquidateSemEq solidityLiquidate)
    (s : MorphoState) (id : Id) (borrower : Address)
    (seizedAssets repaidShares collateralPrice lltvParam lltv : Uint256)
    (seized repaid : Uint256) (s' : MorphoState)
    (h_enabled : s.isLltvEnabled lltv)
    (h_ok : solidityLiquidate s id borrower seizedAssets repaidShares collateralPrice lltvParam =
      some (seized, repaid, s')) :
    s'.isLltvEnabled lltv := by
  have h_ok_morpho : Morpho.liquidate s id borrower seizedAssets repaidShares collateralPrice lltvParam =
      some (seized, repaid, s') := by
    simpa [liquidateSemEq] using
      (h_eq s id borrower seizedAssets repaidShares collateralPrice lltvParam).symm.trans h_ok
  exact liquidate_preserves_lltvMonotone
    s id borrower seizedAssets repaidShares collateralPrice lltvParam lltv h_enabled h_ok_morpho

end Morpho.Proofs.SolidityBridge
