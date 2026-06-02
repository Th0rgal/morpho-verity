/-
  RCF — normal-mode liquidation recovery close factor.

  Solidity anchor: `midnight/src/Midnight.sol` lines 655-667.

  In normal mode, Midnight computes:

    maxRepaid = ceil((debt - maxDebt) * WAD^2 / (WAD^2 - lif * lltv))

  when `lltv < WAD`, and accepts a liquidation if `repaidUnits <= maxRepaid`
  unless the threshold deactivation branch applies. The theorem below captures
  the positive direction needed by the audit question: the RCF cap is high enough
  that choosing `repaidUnits = maxRepaid` can put the account back to healthy.
-/

import Midnight.Proofs.Basic

namespace Midnight.Proofs.RCF

open Midnight.Proofs.Basic

/--
  Health projection read by `isHealthy`: the account is healthy when the
  collateral-derived maximum debt covers the debt.

  This compresses the Solidity collateral loop into its result (`maxDebt`) so the
  proof can focus on the RCF equation while still using the same health predicate.
-/
structure HealthState where
  debt : Nat
  maxDebt : Nat
deriving Repr, DecidableEq

def healthy (s : HealthState) : Prop := s.maxDebt ≥ s.debt

/-- Rounded health predicate used to mirror the Solidity test tolerance around
    `maxRepaid` (`remainingDebt <= newMaxDebt + 3` in `LiquidationTest.sol`). -/
def healthyWithin (slack : Nat) (s : HealthState) : Prop :=
  s.debt ≤ s.maxDebt + slack

/--
  Normal-mode RCF parameters for one liquidated collateral.

  `lif` and `lltv` are the selected collateral's `maxLif` and `lltv`.
  The denominator is exactly `WAD * WAD - lif * lltv` from `Midnight.sol`.
-/
structure RCFParams where
  lif : Nat
  lltv : Nat
deriving Repr, DecidableEq

def scale : Nat := WAD * WAD

def denominator (p : RCFParams) : Nat := scale - p.lif * p.lltv

def debtDecreaseCoeff (p : RCFParams) : Nat := p.lif * p.lltv

theorem denominator_pos_of_coeffValid
    (p : RCFParams) (hcoeff : debtDecreaseCoeff p < scale) :
    denominator p > 0 := by
  unfold denominator debtDecreaseCoeff at *
  omega

/-- Solidity-shaped normal-mode `maxRepaid` expression, in loan units.

    The branch condition follows Midnight: `lltv < WAD` selects the formula.
    Proofs that use the formula arithmetically separately require
    `lif * lltv < WAD^2`, which is the safe-denominator domain for the natural
    number model. -/
def maxRepaid (p : RCFParams) (s : HealthState) : Nat :=
  if p.lltv < WAD then
    mulDivUp (s.debt - s.maxDebt) scale (denominator p)
  else
    UINT256_MAX

theorem maxRepaid_eq_solidity_formula
    (p : RCFParams) (s : HealthState)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale) :
    maxRepaid p s =
      mulDivUp (s.debt - s.maxDebt) scale (denominator p) := by
  have _hden : denominator p > 0 := denominator_pos_of_coeffValid p hcoeff
  simp [maxRepaid, hlltv]

theorem maxRepaid_eq_uint256Max_of_not_lltv
    (p : RCFParams) (s : HealthState)
    (h : ¬ p.lltv < WAD) :
    maxRepaid p s = UINT256_MAX := by
  simp [maxRepaid, h]

def rcfAllows (repaidUnits maxRepaid collateralRepayCapacity rcfThreshold : Nat) :
    Prop :=
  repaidUnits ≤ maxRepaid ∨ collateralRepayCapacity - maxRepaid < rcfThreshold

def zeroFloorSub (x y : Nat) : Nat :=
  if x > y then x - y else 0

def rcfAllowsZeroFloor
    (repaidUnits maxRepaid collateralRepayCapacity rcfThreshold : Nat) :
    Prop :=
  repaidUnits ≤ maxRepaid ∨ zeroFloorSub collateralRepayCapacity maxRepaid < rcfThreshold

theorem rcfAllows_maxRepaid
    (maxRepaid collateralRepayCapacity rcfThreshold : Nat) :
    rcfAllows maxRepaid maxRepaid collateralRepayCapacity rcfThreshold := by
  unfold rcfAllows
  exact Or.inl (le_refl maxRepaid)

theorem rcfAllowsZeroFloor_maxRepaid
    (maxRepaid collateralRepayCapacity rcfThreshold : Nat) :
    rcfAllowsZeroFloor maxRepaid maxRepaid collateralRepayCapacity rcfThreshold := by
  unfold rcfAllowsZeroFloor
  exact Or.inl (le_refl maxRepaid)

theorem maxRepaid_le_debt_of_coeff_debt_le_maxDebt
    (p : RCFParams) (s : HealthState)
    (hdebt : s.maxDebt ≤ s.debt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hbounded : s.debt * debtDecreaseCoeff p ≤ s.maxDebt * scale) :
    maxRepaid p s ≤ s.debt := by
  set excess := s.debt - s.maxDebt
  set coeff := debtDecreaseCoeff p
  have hcoeffRaw : p.lif * p.lltv < scale := by
    simpa [coeff, debtDecreaseCoeff] using hcoeff
  have hdenpos : 0 < denominator p := by
    unfold denominator
    omega
  have hr :
      maxRepaid p s = mulDivUp excess scale (denominator p) := by
    simp [maxRepaid, excess, hlltv]
  rw [hr]
  unfold mulDivUp
  rw [ceilDiv_le_iff hdenpos]
  have hden_coeff : denominator p + coeff = scale := by
    unfold denominator coeff debtDecreaseCoeff
    omega
  have htarget : excess * scale ≤ s.debt * denominator p := by
    have hdebt_eq : s.debt = s.maxDebt + excess := by
      unfold excess
      omega
    nlinarith
  simpa [Nat.mul_comm excess scale] using htarget

theorem maxRepaid_pos_of_unhealthy
    (p : RCFParams) (s : HealthState)
    (hunhealthy : s.debt > s.maxDebt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale) :
    maxRepaid p s > 0 := by
  set excess := s.debt - s.maxDebt
  have hcoeffRaw : p.lif * p.lltv < scale := by
    simpa [debtDecreaseCoeff] using hcoeff
  have hdenpos : 0 < denominator p := by
    unfold denominator
    omega
  have hexcess : 0 < excess := by
    unfold excess
    omega
  have hscale : 0 < scale := by
    unfold scale WAD
    norm_num
  have hr :
      maxRepaid p s = mulDivUp excess scale (denominator p) := by
    simp [maxRepaid, excess, hlltv]
  rw [hr]
  unfold mulDivUp
  exact Nat.div_pos
    (by
      have hnumpos : 0 < excess * scale := Nat.mul_pos hexcess hscale
      omega)
    hdenpos

/--
  Solidity-shaped post-state for a repay-input normal liquidation.

  `maxDebtDecrease` is the decrease in the `isHealthy` collateral loop caused by
  the seized collateral. In the contract it is induced by:

    seizedAssets = repaidUnits.mulDivDown(lif, WAD)
                              .mulDivDown(ORACLE_PRICE_SCALE, price)

  followed by the same collateral-value and LLTV floors used by `isHealthy`.
  Keeping the decrease as a field isolates the only non-trivial rounding bridge.
-/
def liquidateWithDecrease (s : HealthState) (repaidUnits maxDebtDecrease : Nat) :
    HealthState :=
  { debt := s.debt - repaidUnits, maxDebt := s.maxDebt - maxDebtDecrease }

/--
  The exact proof obligation left by Solidity's nested floor arithmetic:
  the selected `repaidUnits` leaves enough health buffer after the collateral
  max-debt decrease.

  For `repaidUnits = maxRepaid`, this is the rounded form of:

    debt - repaid <= maxDebt - repaid * lif * lltv / WAD^2.

  The current file proves that once this arithmetic fact is available, the RCF
  mechanism itself cannot block recovery.
-/
def RoundingSafeForRecovery (s : HealthState) (repaidUnits maxDebtDecrease : Nat) :
    Prop :=
  s.debt ≤ s.maxDebt - maxDebtDecrease + repaidUnits

def RoundingSafeForRecoveryWithin
    (s : HealthState) (repaidUnits maxDebtDecrease slack : Nat) : Prop :=
  s.debt ≤ s.maxDebt - maxDebtDecrease + repaidUnits + slack

def collateralQuote (collateral price : Nat) : Nat :=
  mulDivDown collateral price ORACLE_PRICE_SCALE

def collateralRepayCapacity (collateral price lif : Nat) : Nat :=
  mulDivDown (collateralQuote collateral price) WAD lif

theorem rcfAllows_maxRepaid_with_collateral_capacity
    (maxRepaid collateral price lif rcfThreshold : Nat) :
    rcfAllows maxRepaid maxRepaid
      (collateralRepayCapacity collateral price lif) rcfThreshold :=
  rcfAllows_maxRepaid maxRepaid (collateralRepayCapacity collateral price lif) rcfThreshold

def collateralMaxDebt (collateral price lltv : Nat) : Nat :=
  mulDivDown (collateralQuote collateral price) lltv WAD

theorem collateralMaxDebt_mono_collateral
    {collateral₁ collateral₂ price lltv : Nat} (h : collateral₁ ≤ collateral₂) :
    collateralMaxDebt collateral₁ price lltv ≤
      collateralMaxDebt collateral₂ price lltv := by
  unfold collateralMaxDebt collateralQuote
  exact mulDivDown_mono_left (mulDivDown_mono_left h)

def maxDebtAfterCollateralLoop (selectedContribution : Nat) : List Nat → Nat
  | [] => selectedContribution
  | contribution :: rest => contribution + maxDebtAfterCollateralLoop selectedContribution rest

def seizedAssetsFromRepayValue (repayValue price : Nat) : Nat :=
  mulDivDown repayValue ORACLE_PRICE_SCALE price

def maxDebtDecreaseFromSeizure (collateral seized price lltv : Nat) : Nat :=
  collateralMaxDebt collateral price lltv -
    collateralMaxDebt (collateral - seized) price lltv

def normalModeMaxRepaidProjection
    (p : RCFParams) (debt collateral price : Nat) (otherContributions : List Nat) :
    HealthState :=
  let s : HealthState :=
    { debt := debt,
      maxDebt := maxDebtAfterCollateralLoop
        (collateralMaxDebt collateral price p.lltv) otherContributions }
  let repaid := maxRepaid p s
  let repayValue := mulDivDown repaid p.lif WAD
  let seized := seizedAssetsFromRepayValue repayValue price
  liquidateWithDecrease s repaid
    (maxDebtDecreaseFromSeizure collateral seized price p.lltv)

theorem selectedContribution_le_maxDebtAfterCollateralLoop
    (selectedContribution : Nat) (otherContributions : List Nat) :
    selectedContribution ≤ maxDebtAfterCollateralLoop selectedContribution otherContributions := by
  induction otherContributions with
  | nil => simp [maxDebtAfterCollateralLoop]
  | cons contribution rest ih =>
      simp [maxDebtAfterCollateralLoop]
      exact le_trans ih (Nat.le_add_left _ _)

theorem maxDebtAfterCollateralLoop_replace_selected_sub
    (oldSelected newSelected : Nat) (otherContributions : List Nat)
    (hnew : newSelected ≤ oldSelected) :
    maxDebtAfterCollateralLoop oldSelected otherContributions -
        (oldSelected - newSelected) =
      maxDebtAfterCollateralLoop newSelected otherContributions := by
  induction otherContributions with
  | nil =>
      simp [maxDebtAfterCollateralLoop]
      omega
  | cons contribution rest ih =>
      simp [maxDebtAfterCollateralLoop]
      have hdiff :
          oldSelected - newSelected ≤
            maxDebtAfterCollateralLoop oldSelected rest := by
        exact le_trans (Nat.sub_le oldSelected newSelected)
          (selectedContribution_le_maxDebtAfterCollateralLoop oldSelected rest)
      rw [Nat.add_sub_assoc hdiff contribution, ih]

theorem maxDebtDecreaseFromSeizure_le_collateralMaxDebt
    (collateral seized price lltv : Nat) :
    maxDebtDecreaseFromSeizure collateral seized price lltv ≤
      collateralMaxDebt collateral price lltv := by
  unfold maxDebtDecreaseFromSeizure
  exact Nat.sub_le _ _

theorem maxDebtDecreaseFromSeizure_le_loop
    (collateral seized price lltv : Nat) (otherContributions : List Nat) (sMaxDebt : Nat)
    (hloop :
      sMaxDebt = maxDebtAfterCollateralLoop
        (collateralMaxDebt collateral price lltv) otherContributions) :
    maxDebtDecreaseFromSeizure collateral seized price lltv ≤ sMaxDebt := by
  calc
    maxDebtDecreaseFromSeizure collateral seized price lltv
        ≤ collateralMaxDebt collateral price lltv :=
          maxDebtDecreaseFromSeizure_le_collateralMaxDebt collateral seized price lltv
    _ ≤ maxDebtAfterCollateralLoop
          (collateralMaxDebt collateral price lltv) otherContributions :=
          selectedContribution_le_maxDebtAfterCollateralLoop
            (collateralMaxDebt collateral price lltv) otherContributions
    _ = sMaxDebt := by rw [← hloop]

theorem seizedAssetsFromRepayValue_quoted_le
    (repayValue price : Nat) :
    mulDivUp (seizedAssetsFromRepayValue repayValue price) price ORACLE_PRICE_SCALE ≤
      repayValue := by
  rw [mulDivUp, ceilDiv_le_iff (by unfold ORACLE_PRICE_SCALE; omega)]
  calc seizedAssetsFromRepayValue repayValue price * price
      ≤ repayValue * ORACLE_PRICE_SCALE := by
        simpa [seizedAssetsFromRepayValue] using
          mulDivDown_mul_le repayValue ORACLE_PRICE_SCALE price

theorem repayValue_le_collateralQuote_of_repaid_le_capacity
    (repaidUnits collateral price lif : Nat)
    (hcap : repaidUnits ≤ collateralRepayCapacity collateral price lif) :
    mulDivDown repaidUnits lif WAD ≤ collateralQuote collateral price := by
  by_cases hlif : lif = 0
  · simp [mulDivDown, hlif]
  · have hlifPos : 0 < lif := Nat.pos_of_ne_zero hlif
    have hmul :
        repaidUnits * lif ≤ collateralQuote collateral price * WAD := by
      calc
        repaidUnits * lif
            ≤ collateralRepayCapacity collateral price lif * lif :=
              Nat.mul_le_mul_right lif hcap
        _ = ((collateralQuote collateral price * WAD) / lif) * lif := by
              rfl
        _ ≤ collateralQuote collateral price * WAD :=
              Nat.div_mul_le_self _ _
    calc
      mulDivDown repaidUnits lif WAD
          = repaidUnits * lif / WAD := rfl
      _ ≤ (collateralQuote collateral price * WAD) / WAD :=
          Nat.div_le_div_right hmul
      _ = collateralQuote collateral price := by
          rw [Nat.mul_comm (collateralQuote collateral price) WAD]
          exact Nat.mul_div_right (collateralQuote collateral price)
            (by unfold WAD; omega)

theorem seizedAssetsFromRepayValue_le_collateral_of_repayValue_le_quote
    (collateral price repayValue : Nat)
    (hquote : repayValue ≤ collateralQuote collateral price) :
    seizedAssetsFromRepayValue repayValue price ≤ collateral := by
  by_cases hprice : price = 0
  · simp [seizedAssetsFromRepayValue, mulDivDown, hprice]
  · have hpricePos : 0 < price := Nat.pos_of_ne_zero hprice
    exact Nat.le_of_mul_le_mul_right
      (calc
        seizedAssetsFromRepayValue repayValue price * price
            ≤ repayValue * ORACLE_PRICE_SCALE :=
              mulDivDown_mul_le repayValue ORACLE_PRICE_SCALE price
        _ ≤ collateralQuote collateral price * ORACLE_PRICE_SCALE :=
              Nat.mul_le_mul_right ORACLE_PRICE_SCALE hquote
        _ = ((collateral * price) / ORACLE_PRICE_SCALE) * ORACLE_PRICE_SCALE := by
              rfl
        _ ≤ collateral * price :=
              Nat.div_mul_le_self _ _)
      hpricePos

theorem seizedAssetsFromRepayValue_le_collateral_of_repaid_le_capacity
    (repaidUnits collateral price lif : Nat)
    (hcap : repaidUnits ≤ collateralRepayCapacity collateral price lif) :
    seizedAssetsFromRepayValue (mulDivDown repaidUnits lif WAD) price ≤ collateral :=
  seizedAssetsFromRepayValue_le_collateral_of_repayValue_le_quote collateral price
    (mulDivDown repaidUnits lif WAD)
    (repayValue_le_collateralQuote_of_repaid_le_capacity repaidUnits collateral price lif hcap)

theorem mulDivDown_decrease_le_mulDivUp
    (x y k d : Nat) (hyx : y ≤ x) (hd : 0 < d) :
    mulDivDown x k d - mulDivDown y k d ≤ mulDivUp (x - y) k d := by
  set fy := mulDivDown y k d
  set cd := mulDivUp (x - y) k d
  have hy_floor : y * k < (fy + 1) * d := by
    have hdiv : y * k / d < fy + 1 := by
      simp [fy, mulDivDown]
    exact (Nat.div_lt_iff_lt_mul hd).mp hdiv
  have hcd : (x - y) * k ≤ cd * d := by
    simpa [cd] using le_mulDivUp_mul (a := x - y) (b := k) (d := d) hd
  have hx_lt : x * k < (fy + cd + 1) * d := by
    have hx_split : x * k = y * k + (x - y) * k := by
      rcases exists_add_of_le hyx with ⟨z, hz⟩
      subst hz
      simp [Nat.add_mul]
    rw [hx_split]
    calc y * k + (x - y) * k
        < (fy + 1) * d + cd * d := Nat.add_lt_add_of_lt_of_le hy_floor hcd
      _ = (fy + cd + 1) * d := by ring
  have hfx : mulDivDown x k d < fy + cd + 1 := by
    unfold mulDivDown
    exact (Nat.div_lt_iff_lt_mul hd).mpr hx_lt
  have hle : mulDivDown x k d ≤ fy + cd := by omega
  unfold fy cd at hle
  omega

theorem collateralQuote_decrease_le_quoted_seized
    (collateral seized price : Nat) (hseized : seized ≤ collateral) :
    collateralQuote collateral price - collateralQuote (collateral - seized) price ≤
      mulDivUp seized price ORACLE_PRICE_SCALE := by
  have hmono : collateral - seized ≤ collateral := Nat.sub_le _ _
  have h := mulDivDown_decrease_le_mulDivUp collateral (collateral - seized)
    price ORACLE_PRICE_SCALE hmono (by unfold ORACLE_PRICE_SCALE; omega)
  have hsub : collateral - (collateral - seized) = seized := by omega
  simpa [collateralQuote, hsub] using h

theorem collateralMaxDebt_decrease_le_from_quoted_seized
    (collateral seized price lltv seizedQuote : Nat)
    (hseized : seized ≤ collateral)
    (hquote : mulDivUp seized price ORACLE_PRICE_SCALE ≤ seizedQuote) :
    collateralMaxDebt collateral price lltv -
        collateralMaxDebt (collateral - seized) price lltv ≤
      mulDivUp seizedQuote lltv WAD := by
  set qBefore := collateralQuote collateral price
  set qAfter := collateralQuote (collateral - seized) price
  have hqAfterBefore : qAfter ≤ qBefore := by
    unfold qAfter qBefore collateralQuote
    exact mulDivDown_mono_left (Nat.sub_le _ _)
  have hqDec :
      qBefore - qAfter ≤ seizedQuote := by
    have hraw := collateralQuote_decrease_le_quoted_seized collateral seized price hseized
    exact le_trans (by simpa [qBefore, qAfter] using hraw) hquote
  have hdecr := mulDivDown_decrease_le_mulDivUp qBefore qAfter lltv WAD
    hqAfterBefore (by unfold WAD; omega)
  have hceilMono :
      mulDivUp (qBefore - qAfter) lltv WAD ≤ mulDivUp seizedQuote lltv WAD := by
    unfold mulDivUp
    exact Nat.div_le_div_right (Nat.add_le_add_right (Nat.mul_le_mul_right lltv hqDec) _)
  calc collateralMaxDebt collateral price lltv -
        collateralMaxDebt (collateral - seized) price lltv
      = mulDivDown qBefore lltv WAD - mulDivDown qAfter lltv WAD := by
          rfl
    _ ≤ mulDivUp (qBefore - qAfter) lltv WAD := hdecr
    _ ≤ mulDivUp seizedQuote lltv WAD := hceilMono

/--
  Collapse the nested repay-side floors into the single RCF coefficient, with one
  unit of slack. This mirrors the Solidity path:

    `repaid.mulDivDown(lif, WAD)` then applying `lltv / WAD`

  against the RCF coefficient `lif * lltv / WAD^2`.
-/
theorem nestedRepayCoeff_le_coeff_plus_one (r lif lltv : Nat) :
    mulDivUp (mulDivDown r lif WAD) lltv WAD ≤
      mulDivDown r (lif * lltv) scale + 1 := by
  set a := mulDivDown r lif WAD
  set m := mulDivDown r (lif * lltv) scale
  have hW : 0 < WAD := by
    unfold WAD
    omega
  have hscale : 0 < scale := by
    unfold scale WAD
    omega
  rw [show mulDivUp a lltv WAD = (a * lltv + (WAD - 1)) / WAD by rfl]
  rw [ceilDiv_le_iff hW]
  have ha : a * WAD ≤ r * lif := by
    simpa [a] using mulDivDown_mul_le r lif WAD
  have hmul : (a * lltv) * WAD ≤ r * (lif * lltv) := by
    calc (a * lltv) * WAD
        = (a * WAD) * lltv := by ring
      _ ≤ (r * lif) * lltv := Nat.mul_le_mul_right lltv ha
      _ = r * (lif * lltv) := by ring
  have hm_lt : r * (lif * lltv) < (m + 1) * scale := by
    have hdiv : r * (lif * lltv) / scale < m + 1 := by
      simp [m, mulDivDown]
    exact (Nat.div_lt_iff_lt_mul hscale).mp hdiv
  have hlt : (a * lltv) * WAD < ((m + 1) * WAD) * WAD := by
    have hscale_eq : (m + 1) * scale = ((m + 1) * WAD) * WAD := by
      unfold scale
      ring
    rw [← hscale_eq]
    exact lt_of_le_of_lt hmul hm_lt
  exact le_of_lt (Nat.lt_of_mul_lt_mul_right hlt)

/--
  `maxRepaid` is high enough in the ideal RCF arithmetic.

  The only remaining local rounding obligation is that the actual `isHealthy`
  max-debt decrease caused by seizing collateral is bounded by the one-step
  coefficient `floor(repaidUnits * lif * lltv / WAD^2)`. This is closer to the
  Solidity code than the previous post-state obligation: it isolates exactly the
  nested floor path from `seizedAssets` back into the health loop.
-/
theorem maxRepaid_roundingSafe_of_decrease_le_coeff
    (p : RCFParams) (s : HealthState) (maxDebtDecrease : Nat)
    (hdebt : s.maxDebt ≤ s.debt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hmaxDebtDecrease : maxDebtDecrease ≤ s.maxDebt)
    (hdecreaseCoeff :
      maxDebtDecrease ≤ mulDivDown (maxRepaid p s) (debtDecreaseCoeff p) scale) :
    RoundingSafeForRecovery s (maxRepaid p s) maxDebtDecrease := by
  set excess := s.debt - s.maxDebt
  set r := maxRepaid p s
  set coeff := debtDecreaseCoeff p
  have hscale : 0 < scale := by
    unfold scale WAD
    omega
  have hcoeffLe : coeff ≤ scale := le_of_lt hcoeff
  have hcoeffRaw : p.lif * p.lltv < scale := by
    simpa [coeff, debtDecreaseCoeff] using hcoeff
  have hdenpos : 0 < denominator p := by
    unfold denominator
    omega
  have hr :
      r = mulDivUp excess scale (denominator p) := by
    simp [r, maxRepaid, excess, hlltv]
  have hceil : excess * scale ≤ r * denominator p := by
    rw [hr]
    exact le_mulDivUp_mul hdenpos
  have hdecScale : maxDebtDecrease * scale ≤ r * coeff := by
    calc maxDebtDecrease * scale
        ≤ mulDivDown r coeff scale * scale :=
            Nat.mul_le_mul_right scale hdecreaseCoeff
      _ ≤ r * coeff := by
            exact mulDivDown_mul_le r coeff scale
  have hsum : (excess + maxDebtDecrease) * scale ≤ r * scale := by
    calc (excess + maxDebtDecrease) * scale
        = excess * scale + maxDebtDecrease * scale := by ring
      _ ≤ r * denominator p + r * coeff := Nat.add_le_add hceil hdecScale
      _ = r * scale := by
        unfold denominator coeff debtDecreaseCoeff
        rw [← Nat.mul_add]
        have hsub : scale - p.lif * p.lltv + p.lif * p.lltv = scale := by
          omega
        rw [hsub]
  have hexcess_dec_le_r : excess + maxDebtDecrease ≤ r :=
    Nat.le_of_mul_le_mul_right hsum hscale
  unfold RoundingSafeForRecovery
  have hdebt_eq : s.debt = s.maxDebt + excess := by
    omega
  rw [hdebt_eq]
  omega

theorem maxRepaid_roundingSafeWithin_of_decrease_le_coeff_with_slack
    (p : RCFParams) (s : HealthState) (maxDebtDecrease slack : Nat)
    (hdebt : s.maxDebt ≤ s.debt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hmaxDebtDecrease : maxDebtDecrease ≤ s.maxDebt)
    (hdecreaseCoeff :
      maxDebtDecrease ≤
        mulDivDown (maxRepaid p s) (debtDecreaseCoeff p) scale + slack) :
    RoundingSafeForRecoveryWithin s (maxRepaid p s) maxDebtDecrease slack := by
  set excess := s.debt - s.maxDebt
  set r := maxRepaid p s
  set coeff := debtDecreaseCoeff p
  have hscale : 0 < scale := by
    unfold scale WAD
    omega
  have hcoeffRaw : p.lif * p.lltv < scale := by
    simpa [coeff, debtDecreaseCoeff] using hcoeff
  have hdenpos : 0 < denominator p := by
    unfold denominator
    omega
  have hr :
      r = mulDivUp excess scale (denominator p) := by
    simp [r, maxRepaid, excess, hlltv]
  have hceil : excess * scale ≤ r * denominator p := by
    rw [hr]
    exact le_mulDivUp_mul hdenpos
  have hdecScale :
      maxDebtDecrease * scale ≤ r * coeff + slack * scale := by
    calc maxDebtDecrease * scale
        ≤ (mulDivDown r coeff scale + slack) * scale :=
            Nat.mul_le_mul_right scale hdecreaseCoeff
      _ = mulDivDown r coeff scale * scale + slack * scale := by ring
      _ ≤ r * coeff + slack * scale := by
            exact Nat.add_le_add_right (mulDivDown_mul_le r coeff scale) _
  have hsum : (excess + maxDebtDecrease) * scale ≤ (r + slack) * scale := by
    calc (excess + maxDebtDecrease) * scale
        = excess * scale + maxDebtDecrease * scale := by ring
      _ ≤ r * denominator p + (r * coeff + slack * scale) :=
            Nat.add_le_add hceil hdecScale
      _ = (r + slack) * scale := by
        have hden_coeff : denominator p + coeff = scale := by
          unfold denominator coeff debtDecreaseCoeff
          omega
        calc r * denominator p + (r * coeff + slack * scale)
            = r * (denominator p + coeff) + slack * scale := by ring
          _ = r * scale + slack * scale := by rw [hden_coeff]
          _ = (r + slack) * scale := by ring
  have hexcess_dec_le_r_slack : excess + maxDebtDecrease ≤ r + slack :=
    Nat.le_of_mul_le_mul_right hsum hscale
  unfold RoundingSafeForRecoveryWithin
  have hdebt_eq : s.debt = s.maxDebt + excess := by
    omega
  rw [hdebt_eq]
  omega

theorem maxRepaid_roundingSafeWithin_of_nested_decrease_bound
    (p : RCFParams) (s : HealthState) (maxDebtDecrease : Nat)
    (hdebt : s.maxDebt ≤ s.debt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hmaxDebtDecrease : maxDebtDecrease ≤ s.maxDebt)
    (hnested :
      maxDebtDecrease ≤
        mulDivUp (mulDivDown (maxRepaid p s) p.lif WAD) p.lltv WAD) :
    RoundingSafeForRecoveryWithin s (maxRepaid p s) maxDebtDecrease 1 :=
  maxRepaid_roundingSafeWithin_of_decrease_le_coeff_with_slack p s
    maxDebtDecrease 1 hdebt hlltv hcoeff hmaxDebtDecrease
    (le_trans hnested
      (by
        simpa [debtDecreaseCoeff] using
          nestedRepayCoeff_le_coeff_plus_one (maxRepaid p s) p.lif p.lltv))

theorem healthy_after_repay_of_roundingSafe
    (s : HealthState) (repaidUnits maxDebtDecrease : Nat)
    (_hrepaid : repaidUnits ≤ s.debt)
    (_hmaxDebtDecrease : maxDebtDecrease ≤ s.maxDebt)
    (_hdecrease : maxDebtDecrease ≤ repaidUnits)
    (hsafe : RoundingSafeForRecovery s repaidUnits maxDebtDecrease) :
    healthy (liquidateWithDecrease s repaidUnits maxDebtDecrease) := by
  simpa [healthy, liquidateWithDecrease, RoundingSafeForRecovery] using hsafe

theorem healthyWithin_after_repay_of_roundingSafeWithin
    (s : HealthState) (repaidUnits maxDebtDecrease slack : Nat)
    (hsafe : RoundingSafeForRecoveryWithin s repaidUnits maxDebtDecrease slack) :
    healthyWithin slack (liquidateWithDecrease s repaidUnits maxDebtDecrease) := by
  unfold RoundingSafeForRecoveryWithin at hsafe
  simp [healthyWithin, liquidateWithDecrease]
  omega

/--
  Main RCF theorem.

  If the liquidation uses `repaidUnits = maxRepaid` and the Solidity
  seized-collateral rounding satisfies `RoundingSafeForRecovery`, the borrower's
  post-liquidation account is healthy. This is the formal statement that the RCF
  cap does not prevent choosing a recovery-sized liquidation.
-/
theorem rcf_maxRepaid_can_restore_health
    (p : RCFParams) (s : HealthState) (maxDebtDecrease : Nat)
    (hrepaid : maxRepaid p s ≤ s.debt)
    (hmaxDebtDecrease : maxDebtDecrease ≤ s.maxDebt)
    (hdecrease : maxDebtDecrease ≤ maxRepaid p s)
    (hsafe : RoundingSafeForRecovery s (maxRepaid p s) maxDebtDecrease) :
    healthy (liquidateWithDecrease s (maxRepaid p s) maxDebtDecrease) :=
  healthy_after_repay_of_roundingSafe s (maxRepaid p s) maxDebtDecrease
    hrepaid hmaxDebtDecrease hdecrease hsafe

/--
  A stronger, code-review-friendly corollary: the theorem applies directly to
  an unhealthy pre-state. The proof does not need unhealthiness; it is included
  because normal-mode `liquidate` requires `originalDebt > maxDebt`.
-/
theorem rcf_maxRepaid_restores_unhealthy
    (p : RCFParams) (s : HealthState) (maxDebtDecrease : Nat)
    (_hunhealthy : s.debt > s.maxDebt)
    (hrepaid : maxRepaid p s ≤ s.debt)
    (hmaxDebtDecrease : maxDebtDecrease ≤ s.maxDebt)
    (hdecrease : maxDebtDecrease ≤ maxRepaid p s)
    (hsafe : RoundingSafeForRecovery s (maxRepaid p s) maxDebtDecrease) :
    healthy (liquidateWithDecrease s (maxRepaid p s) maxDebtDecrease) :=
  rcf_maxRepaid_can_restore_health p s maxDebtDecrease
    hrepaid hmaxDebtDecrease hdecrease hsafe

/--
  Stronger RCF corollary using the `maxRepaid` formula plus the local
  coefficient-decrease rounding bound, instead of assuming the final post-state
  recovery inequality directly.
-/
theorem rcf_maxRepaid_restores_unhealthy_of_decrease_le_coeff
    (p : RCFParams) (s : HealthState) (maxDebtDecrease : Nat)
    (hunhealthy : s.debt > s.maxDebt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hrepaid : maxRepaid p s ≤ s.debt)
    (hmaxDebtDecrease : maxDebtDecrease ≤ s.maxDebt)
    (hdecrease : maxDebtDecrease ≤ maxRepaid p s)
    (hdecreaseCoeff :
      maxDebtDecrease ≤ mulDivDown (maxRepaid p s) (debtDecreaseCoeff p) scale) :
    healthy (liquidateWithDecrease s (maxRepaid p s) maxDebtDecrease) :=
  rcf_maxRepaid_restores_unhealthy p s maxDebtDecrease hunhealthy
    hrepaid hmaxDebtDecrease hdecrease
    (maxRepaid_roundingSafe_of_decrease_le_coeff p s maxDebtDecrease
      (le_of_lt hunhealthy) hlltv hcoeff hmaxDebtDecrease hdecreaseCoeff)

/--
  Faithful rounded RCF corollary. Solidity's own regression test allows a small
  health slack after `maxRepaid` because nested floors can leave the position
  "almost healthy"; this theorem exposes that slack explicitly.
-/
theorem rcf_maxRepaid_restores_unhealthy_with_slack
    (p : RCFParams) (s : HealthState) (maxDebtDecrease slack : Nat)
    (hunhealthy : s.debt > s.maxDebt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hmaxDebtDecrease : maxDebtDecrease ≤ s.maxDebt)
    (hdecreaseCoeff :
      maxDebtDecrease ≤
        mulDivDown (maxRepaid p s) (debtDecreaseCoeff p) scale + slack) :
    healthyWithin slack
      (liquidateWithDecrease s (maxRepaid p s) maxDebtDecrease) :=
  healthyWithin_after_repay_of_roundingSafeWithin s (maxRepaid p s)
    maxDebtDecrease slack
    (maxRepaid_roundingSafeWithin_of_decrease_le_coeff_with_slack p s
      maxDebtDecrease slack (le_of_lt hunhealthy) hlltv hcoeff
      hmaxDebtDecrease hdecreaseCoeff)

/--
  RCF corollary for the nested repay-side Solidity arithmetic. The remaining
  premise is now the collateral-health-loop fact that the actual max-debt
  decrease is bounded by the nested repay-side amount.
-/
theorem rcf_maxRepaid_restores_unhealthy_with_one_slack_of_nested_decrease
    (p : RCFParams) (s : HealthState) (maxDebtDecrease : Nat)
    (hunhealthy : s.debt > s.maxDebt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hmaxDebtDecrease : maxDebtDecrease ≤ s.maxDebt)
    (hnested :
      maxDebtDecrease ≤
        mulDivUp (mulDivDown (maxRepaid p s) p.lif WAD) p.lltv WAD) :
    healthyWithin 1
      (liquidateWithDecrease s (maxRepaid p s) maxDebtDecrease) :=
  healthyWithin_after_repay_of_roundingSafeWithin s (maxRepaid p s)
    maxDebtDecrease 1
    (maxRepaid_roundingSafeWithin_of_nested_decrease_bound p s maxDebtDecrease
      (le_of_lt hunhealthy) hlltv hcoeff hmaxDebtDecrease hnested)

/--
  RCF corollary using the actual repaid-input seized-assets formula from
  `Midnight.sol`:

    `repaidValue = repaidUnits.mulDivDown(lif, WAD)`
    `seizedAssets = repaidValue.mulDivDown(ORACLE_PRICE_SCALE, price)`

  The theorem proves rounded recovery with `+1` slack from collapsing the
  repay-side nested floors into the RCF coefficient.
-/
theorem rcf_maxRepaid_restores_unhealthy_with_one_slack_of_collateral_seizure
    (p : RCFParams) (s : HealthState) (collateral price : Nat)
    (hunhealthy : s.debt > s.maxDebt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hmaxDebtDecrease :
      maxDebtDecreaseFromSeizure collateral
        (seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) price)
        price p.lltv ≤ s.maxDebt)
    (hseized :
      seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) price ≤
        collateral) :
    healthyWithin 1
      (liquidateWithDecrease s (maxRepaid p s)
        (maxDebtDecreaseFromSeizure collateral
          (seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) price)
          price p.lltv)) := by
  set repayValue := mulDivDown (maxRepaid p s) p.lif WAD
  set seized := seizedAssetsFromRepayValue repayValue price
  set maxDebtDecrease := maxDebtDecreaseFromSeizure collateral seized price p.lltv
  have hquote : mulDivUp seized price ORACLE_PRICE_SCALE ≤ repayValue := by
    simpa [seized, repayValue] using seizedAssetsFromRepayValue_quoted_le repayValue price
  have hcollateral :
      maxDebtDecrease ≤ mulDivUp repayValue p.lltv WAD := by
    simpa [maxDebtDecrease, maxDebtDecreaseFromSeizure, seized, repayValue] using
      collateralMaxDebt_decrease_le_from_quoted_seized collateral seized price p.lltv
        repayValue (by simpa [seized, repayValue] using hseized) hquote
  have hnested :
      mulDivUp repayValue p.lltv WAD ≤
        mulDivDown (maxRepaid p s) (debtDecreaseCoeff p) scale + 1 := by
    simpa [repayValue, debtDecreaseCoeff] using
      nestedRepayCoeff_le_coeff_plus_one (maxRepaid p s) p.lif p.lltv
  have hdecreaseCoeff :
      maxDebtDecrease ≤
        mulDivDown (maxRepaid p s) (debtDecreaseCoeff p) scale + 1 := by
    omega
  simpa [maxDebtDecrease, seized, repayValue] using
    rcf_maxRepaid_restores_unhealthy_with_slack p s maxDebtDecrease 1
      hunhealthy hlltv hcoeff
      (by simpa [maxDebtDecrease, seized, repayValue] using hmaxDebtDecrease)
      hdecreaseCoeff

/-- Direct counterpart of Midnight's `testMaxRepaidMeansRecovery`, whose
    assertion is `remainingDebt <= newMaxDebt + 3`. The proof is stronger
    internally (`+1`) and weakens to the Solidity test's `+3` tolerance. -/
theorem rcf_maxRepaid_matches_solidity_test_tolerance
    (p : RCFParams) (s : HealthState) (collateral price : Nat)
    (hunhealthy : s.debt > s.maxDebt)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hmaxDebtDecrease :
      maxDebtDecreaseFromSeizure collateral
        (seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) price)
        price p.lltv ≤ s.maxDebt)
    (hseized :
      seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) price ≤
        collateral) :
    healthyWithin 3
      (liquidateWithDecrease s (maxRepaid p s)
        (maxDebtDecreaseFromSeizure collateral
          (seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) price)
          price p.lltv)) := by
  have h1 :=
    rcf_maxRepaid_restores_unhealthy_with_one_slack_of_collateral_seizure
      p s collateral price hunhealthy hlltv hcoeff hmaxDebtDecrease hseized
  unfold healthyWithin at *
  omega

theorem normalModeMaxRepaidProjection_matches_solidity_test_tolerance
    (p : RCFParams) (debt collateral price : Nat) (otherContributions : List Nat)
    (hunhealthy :
      debt >
        maxDebtAfterCollateralLoop
          (collateralMaxDebt collateral price p.lltv) otherContributions)
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hseized :
      seizedAssetsFromRepayValue
          (mulDivDown
            (maxRepaid p
              { debt := debt,
                maxDebt := maxDebtAfterCollateralLoop
                  (collateralMaxDebt collateral price p.lltv) otherContributions })
            p.lif WAD)
          price ≤ collateral) :
    healthyWithin 3
      (normalModeMaxRepaidProjection p debt collateral price otherContributions) := by
  let s : HealthState :=
    { debt := debt,
      maxDebt := maxDebtAfterCollateralLoop
        (collateralMaxDebt collateral price p.lltv) otherContributions }
  have hdecrease :
      maxDebtDecreaseFromSeizure collateral
          (seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) price)
          price p.lltv ≤ s.maxDebt := by
    exact maxDebtDecreaseFromSeizure_le_loop collateral
      (seizedAssetsFromRepayValue (mulDivDown (maxRepaid p s) p.lif WAD) price)
      price p.lltv otherContributions s.maxDebt rfl
  have h := rcf_maxRepaid_matches_solidity_test_tolerance
    p s collateral price
    (by simpa [s] using hunhealthy)
    hlltv hcoeff hdecrease
    (by simpa [s] using hseized)
  simpa [normalModeMaxRepaidProjection, s] using h

theorem normalModeMaxRepaidProjection_singleOther_matches_solidity_test_tolerance
    (p : RCFParams) (debt otherMaxDebt collateral price : Nat)
    (hunhealthy :
      debt >
        maxDebtAfterCollateralLoop
          (collateralMaxDebt collateral price p.lltv) [otherMaxDebt])
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hseized :
      seizedAssetsFromRepayValue
          (mulDivDown
            (maxRepaid p
              { debt := debt,
                maxDebt := maxDebtAfterCollateralLoop
                  (collateralMaxDebt collateral price p.lltv) [otherMaxDebt] })
            p.lif WAD)
          price ≤ collateral) :
    healthyWithin 3
      (normalModeMaxRepaidProjection p debt collateral price [otherMaxDebt]) :=
  normalModeMaxRepaidProjection_matches_solidity_test_tolerance
    p debt collateral price [otherMaxDebt] hunhealthy hlltv hcoeff hseized

def normalModeMaxRepaidHealthyWithin3
    (p : RCFParams) (debt otherMaxDebt collateral price : Nat) : Prop :=
  healthyWithin 3
    (normalModeMaxRepaidProjection p debt collateral price [otherMaxDebt])

theorem normalModeMaxRepaidHealthyWithin3_of_projection_domain
    (p : RCFParams) (debt otherMaxDebt collateral price : Nat)
    (hunhealthy :
      debt >
        maxDebtAfterCollateralLoop
          (collateralMaxDebt collateral price p.lltv) [otherMaxDebt])
    (hlltv : p.lltv < WAD)
    (hcoeff : debtDecreaseCoeff p < scale)
    (hseized :
      seizedAssetsFromRepayValue
          (mulDivDown
            (maxRepaid p
              { debt := debt,
                maxDebt := maxDebtAfterCollateralLoop
                  (collateralMaxDebt collateral price p.lltv) [otherMaxDebt] })
            p.lif WAD)
          price ≤ collateral) :
    normalModeMaxRepaidHealthyWithin3 p debt otherMaxDebt collateral price := by
  unfold normalModeMaxRepaidHealthyWithin3
  exact normalModeMaxRepaidProjection_singleOther_matches_solidity_test_tolerance
    p debt otherMaxDebt collateral price hunhealthy hlltv hcoeff hseized

end Midnight.Proofs.RCF
