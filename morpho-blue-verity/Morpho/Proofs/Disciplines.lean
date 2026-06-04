import Morpho.Proofs.Env
import Morpho.Proofs.Projection
import Morpho.Proofs.HealthFaithful
import Morpho.Contract

namespace Morpho.Proofs.Disciplines

set_option linter.unusedVariables false

open Verity
open Contracts
open Morpho.Proofs.HealthModel
open Morpho.Proofs.HealthModel.HealthState
open Morpho.Proofs.Env
open Morpho.Proofs.Projection
open Morpho.Proofs.HealthFaithful
open Morpho.Contract.Morpho

/-!
  Generated-body discipline interface.

  This module is the boundary between the compact health model and the generated
  Morpho Blue contract bodies. The `Step` definitions below are intentionally
  real executable entrypoint runs projected into `HealthState`; the named
  discipline facts are explicit local obligations over those generated bodies.

  Earlier versions tried to discharge these obligations by deeply traversing the
  generated Lean terms in this file. That proof search currently overflows Lean's
  stack before the module finishes elaborating, so the generated-body obligations
  are kept as typed axioms here until the traversal is factored into smaller
  lemmas or a dedicated symbolic executor.
-/

structure Position where
  mp      : MarketParams
  id      : Bytes32
  account : Address
  price   : Uint256

def marketIdRead (mp : MarketParams) : Contract Bytes32 := do
  ecmCall
    (fun resultVar => Compiler.Modules.Hashing.abiEncodeStaticWordsModule resultVar 5)
    [addressToWord mp.loanToken, addressToWord mp.collateralToken,
      addressToWord mp.oracle, addressToWord mp.irm, mp.lltv]

def oraclePriceRead (mp : MarketParams) : Contract Uint256 := do
  ecmCall
    (fun resultVar => Compiler.Modules.Oracle.oracleReadUint256Module resultVar 0xa035b1fe 0)
    [addressToWord mp.oracle]

def MarketIdAligned (P : Position) : Prop :=
  ∀ cs id cs',
    (marketIdRead P.mp).run cs = ContractResult.success id cs' →
      id = P.id

def OraclePriceAligned (P : Position) : Prop :=
  ∀ cs price cs',
    (oraclePriceRead P.mp).run cs = ContractResult.success price cs' →
      price = P.price

def OraclePriceNoOverflow (P : Position) (cs : ContractState) : Prop :=
  let s := project P.mp P.id P.account P.price cs
  s.collateral * s.price < Verity.Core.Uint256.modulus ∧
  Morpho.Proofs.HealthModel.mulDivDown s.collateral s.price ORACLE_PRICE_SCALE * s.lltv
    < Verity.Core.Uint256.modulus

def LocalNoOverflow (P : Position) (cs : ContractState) : Prop :=
  OraclePriceNoOverflow P cs

def LocalNoOverflowFor (P : Position) : Prop :=
  ∀ cs, LocalNoOverflow P cs

def AccrueInterestIdentityFor (P : Position) : Prop :=
  ∀ out cs cs',
    (_accrueInterest P.mp P.id).run cs = ContractResult.success out cs' →
      project P.mp P.id P.account P.price cs' =
        project P.mp P.id P.account P.price cs

def MonotoneDiscipline (st : Step) : Prop :=
  ∀ s s', st s s' →
    s'.lltv = s.lltv ∧ s.collateral ≤ s'.collateral ∧ s'.borrowShares ≤ s.borrowShares

def GuardedDiscipline (st : Step) : Prop :=
  ∀ s s', st s s' → healthy s'

def supplyStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets shares onBehalf data out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (supply P.mp assets shares onBehalf data).run cs = ContractResult.success out cs'

def withdrawStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets shares onBehalf receiver out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (withdraw P.mp assets shares onBehalf receiver).run cs =
        ContractResult.success out cs'

def supplyCollateralStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets onBehalf data out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (supplyCollateral P.mp assets onBehalf data).run cs =
        ContractResult.success out cs'

def repayStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets shares onBehalf data out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (repay P.mp assets shares onBehalf data).run cs = ContractResult.success out cs'

def borrowStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets shares receiver out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (borrow P.mp assets shares P.account receiver).run cs =
        ContractResult.success out cs'

def withdrawCollateralStep (P : Position) : Step :=
  fun s s' =>
    ∃ assets receiver out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (withdrawCollateral P.mp assets P.account receiver).run cs =
        ContractResult.success out cs'

def liquidateStep (P : Position) : Step :=
  fun s s' =>
    ∃ seized repaid data out cs cs',
      s = project P.mp P.id P.account P.price cs ∧
      s' = project P.mp P.id P.account P.price cs' ∧
      (liquidate P.mp P.account seized repaid data).run cs =
        ContractResult.success out cs'

axiom noOverflow_of_localNoOverflow (P : Position) (cs : ContractState) :
    LocalNoOverflow P cs →
      NoOverflow P.mp P.id P.account P.price cs

axiom monotoneDiscipline_supply (P : Position) :
    MonotoneDiscipline (supplyStep P)

axiom monotoneDiscipline_withdraw (P : Position) :
    MonotoneDiscipline (withdrawStep P)

axiom monotoneDiscipline_supplyCollateral (P : Position) :
    MonotoneDiscipline (supplyCollateralStep P)

axiom monotoneDiscipline_repay (P : Position) :
    MonotoneDiscipline (repayStep P)

axiom guardedDiscipline_borrow (P : Position)
    (hid : MarketIdAligned P) (hprice : OraclePriceAligned P)
    (horacleFits : LocalNoOverflowFor P) :
    GuardedDiscipline (borrowStep P)

axiom guardedDiscipline_withdrawCollateral (P : Position)
    (hid : MarketIdAligned P) (hprice : OraclePriceAligned P)
    (horacleFits : LocalNoOverflowFor P) :
    GuardedDiscipline (withdrawCollateralStep P)

axiom liquidate_guardUnhealthy_afterAccrue_price (P : Position)
    (hid : MarketIdAligned P) (hprice : OraclePriceAligned P) :
    ∀ seized repaid data out cs cs',
      (liquidate P.mp P.account seized repaid data).run cs
          = Verity.ContractResult.success out cs' →
        ∃ stPostAccrue stHealth,
          (_isHealthyWithPrice P.mp P.id P.account P.price).run stPostAccrue
            = Verity.ContractResult.success false stHealth

axiom liquidate_preStateUnhealthy_of_accrueInterest_identity (P : Position)
    (hid : MarketIdAligned P) (hprice : OraclePriceAligned P)
    (hnoAccrual : AccrueInterestIdentityFor P) :
    ∀ seized repaid data out cs cs',
      (liquidate P.mp P.account seized repaid data).run cs
          = Verity.ContractResult.success out cs' →
        ∃ csHealth,
          (_isHealthyWithPrice P.mp P.id P.account P.price).run cs
            = Verity.ContractResult.success false csHealth

end Morpho.Proofs.Disciplines
