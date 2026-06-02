/-
  UnitsAccounting — totalUnits covers up-to-date lender credit after slashing.

  Solidity anchors:
    * bad-debt socialization updates `lossFactor` and decreases `totalUnits`
      (`midnight/src/Midnight.sol` lines 626-640);
    * `updatePositionView` computes post-slash credit, post-slash pending fee,
      and accrued fee from
      `credit * (UINT128_MAX - market.lossFactor)
              / (UINT128_MAX - lastLossFactor)`
      (`Midnight.sol` lines 797-818).

  This file models that accounting relation directly. The key invariant is not
  tied to stale `position.credit`; it is tied to each lender's credit after
  applying the current market loss factor.
-/

import Midnight.Proofs.Basic

namespace Midnight.Proofs.UnitsAccounting

open Midnight.Proofs.Basic

structure Market where
  totalUnits : Nat
  lossFactor : Nat
deriving Repr, DecidableEq

structure Lender where
  credit : Nat
  lastLossFactor : Nat
deriving Repr, DecidableEq

structure LenderPosition where
  lender : Lender
  pendingFee : Nat
  lastAccrual : Nat
deriving Repr, DecidableEq

structure UpdatePositionViewResult where
  credit : Nat
  pendingFee : Nat
  fee : Nat
deriving Repr, DecidableEq

def accrualEnd (timestamp maturity : Nat) : Nat :=
  if timestamp ≤ maturity then timestamp else maturity

/-- Credit after applying the current market loss factor, matching the slashing
    branch of `updatePositionView` and intentionally excluding continuous-fee
    accrual. Fees only reduce lender credit further, so this is the hard side of
    the cover property. -/
def postSlashCredit (m : Market) (l : Lender) : Nat :=
  if l.lastLossFactor < UINT128_MAX then
    mulDivDown l.credit (UINT128_MAX - m.lossFactor) (UINT128_MAX - l.lastLossFactor)
  else
    0

def sumPostSlashCredit (m : Market) : List Lender → Nat
  | [] => 0
  | l :: ls => postSlashCredit m l + sumPostSlashCredit m ls

/-- Pending fee after applying the same loss ratio as credit, matching
    `updatePositionView`'s `postSlashPendingFee` branch. -/
def postSlashPendingFee (m : Market) (p : LenderPosition) : Nat :=
  let postSlashCredit := postSlashCredit m p.lender
  if p.lender.credit > 0 then
    p.pendingFee - mulDivUp p.pendingFee (p.lender.credit - postSlashCredit) p.lender.credit
  else
    0

/-- Accrued continuous fee from the already slashed pending fee. `accrualEnd` is
    the Solidity `min(block.timestamp, market.maturity)` projection. -/
def accruedFeeView (m : Market) (p : LenderPosition) (accrualEnd maturity : Nat) : Nat :=
  if p.lastAccrual < maturity then
    mulDivDown (postSlashPendingFee m p) (accrualEnd - p.lastAccrual)
      (maturity - p.lastAccrual)
  else
    0

/-- Full `updatePositionView` returned credit projection:
    `uint128(postSlashCredit) - fee`. -/
def postUpdateCreditView
    (m : Market) (p : LenderPosition) (accrualEnd maturity : Nat) : Nat :=
  postSlashCredit m p.lender - accruedFeeView m p accrualEnd maturity

def postUpdatePendingFeeView
    (m : Market) (p : LenderPosition) (accrualEnd maturity : Nat) : Nat :=
  postSlashPendingFee m p - accruedFeeView m p accrualEnd maturity

def updatePositionViewResult
    (m : Market) (p : LenderPosition) (timestamp maturity : Nat) :
    UpdatePositionViewResult :=
  let endTime := accrualEnd timestamp maturity
  let fee := accruedFeeView m p endTime maturity
  { credit := postSlashCredit m p.lender - fee,
    pendingFee := postSlashPendingFee m p - fee,
    fee := fee }

theorem updatePositionViewResult_creditEq
    (m : Market) (p : LenderPosition) (timestamp maturity : Nat) :
    (updatePositionViewResult m p timestamp maturity).credit =
      postUpdateCreditView m p (accrualEnd timestamp maturity) maturity := by
  rfl

theorem updatePositionViewResult_pendingFeeEq
    (m : Market) (p : LenderPosition) (timestamp maturity : Nat) :
    (updatePositionViewResult m p timestamp maturity).pendingFee =
      postUpdatePendingFeeView m p (accrualEnd timestamp maturity) maturity := by
  rfl

theorem updatePositionViewResult_feeEq
    (m : Market) (p : LenderPosition) (timestamp maturity : Nat) :
    (updatePositionViewResult m p timestamp maturity).fee =
      accruedFeeView m p (accrualEnd timestamp maturity) maturity := by
  rfl

def sumPostUpdateCreditView (m : Market) (accrualEnd maturity : Nat) :
    List LenderPosition → Nat
  | [] => 0
  | p :: ps =>
      postUpdateCreditView m p accrualEnd maturity +
        sumPostUpdateCreditView m accrualEnd maturity ps

def sumUpdatePositionViewResultCredit
    (m : Market) (timestamp maturity : Nat) : List LenderPosition → Nat
  | [] => 0
  | p :: ps =>
      (updatePositionViewResult m p timestamp maturity).credit +
        sumUpdatePositionViewResultCredit m timestamp maturity ps

theorem sumUpdatePositionViewResultCredit_eq_sumPostUpdateCreditView
    (m : Market) (timestamp maturity : Nat) (positions : List LenderPosition) :
    sumUpdatePositionViewResultCredit m timestamp maturity positions =
      sumPostUpdateCreditView m (accrualEnd timestamp maturity) maturity positions := by
  induction positions with
  | nil => rfl
  | cons p ps ih =>
      simp [sumUpdatePositionViewResultCredit, sumPostUpdateCreditView,
        updatePositionViewResult_creditEq, ih]

def updateViewDomain (m : Market) : List LenderPosition → Prop
  | [] => True
  | p :: ps =>
      p.pendingFee ≤ p.lender.credit ∧
        p.lender.lastLossFactor ≤ m.lossFactor ∧
        updateViewDomain m ps

def updateViewSubtractionsSafe
    (m : Market) (accrualEnd maturity : Nat) : List LenderPosition → Prop
  | [] => True
  | p :: ps =>
      accruedFeeView m p accrualEnd maturity ≤ postSlashCredit m p.lender ∧
        updateViewSubtractionsSafe m accrualEnd maturity ps

def lendersOfPositions : List LenderPosition → List Lender
  | [] => []
  | p :: ps => p.lender :: lendersOfPositions ps

def collateralRepayableValue (collateral price maxLif : Nat) : Nat :=
  mulDivUp (mulDivUp collateral price ORACLE_PRICE_SCALE) WAD maxLif

def collateralRepayableValuesOf : List (Nat × Nat × Nat) → List Nat
  | [] => []
  | (collateral, price, maxLif) :: rest =>
      collateralRepayableValue collateral price maxLif ::
        collateralRepayableValuesOf rest

/-- Projection of `liquidate`'s bad-debt collateral loop:

```solidity
badDebt = originalDebt;
badDebt = badDebt.zeroFloorSub(collateralRepayableValue);
```

The list entries are the per-collateral
`collateral.mulDivUp(price, ORACLE_PRICE_SCALE).mulDivUp(WAD, maxLif)` values.
-/
def zeroFloorSub (x y : Nat) : Nat :=
  if x > y then x - y else 0

theorem zeroFloorSub_eq_sub (x y : Nat) :
    zeroFloorSub x y = x - y := by
  unfold zeroFloorSub
  by_cases h : x > y
  · simp [h]
  · have hle : x ≤ y := Nat.le_of_not_gt h
    simp [h, Nat.sub_eq_zero_of_le hle]

def badDebtAfterCollateralLoop : Nat → List Nat → Nat
  | debt, [] => debt
  | debt, repayable :: rest => badDebtAfterCollateralLoop (debt - repayable) rest

theorem badDebtAfterCollateralLoop_le_start (debt : Nat) (repayableValues : List Nat) :
    badDebtAfterCollateralLoop debt repayableValues ≤ debt := by
  induction repayableValues generalizing debt with
  | nil => simp [badDebtAfterCollateralLoop]
  | cons repayable rest ih =>
      exact le_trans (ih (debt - repayable)) (Nat.sub_le debt repayable)

theorem postUpdateCreditView_le_postSlashCredit
    (m : Market) (p : LenderPosition) (accrualEnd maturity : Nat) :
    postUpdateCreditView m p accrualEnd maturity ≤ postSlashCredit m p.lender := by
  unfold postUpdateCreditView
  exact Nat.sub_le _ _

theorem postSlashCredit_le_credit
    (m : Market) (l : Lender)
    (hadvanced : l.lastLossFactor ≤ m.lossFactor)
    (hm : m.lossFactor ≤ UINT128_MAX) :
    postSlashCredit m l ≤ l.credit := by
  unfold postSlashCredit
  by_cases hlast : l.lastLossFactor < UINT128_MAX
  · simp [hlast]
    apply mulDivDown_le_left
    omega
  · simp [hlast]

theorem postSlashPendingFee_le_postSlashCredit
    (m : Market) (p : LenderPosition)
    (hpending : p.pendingFee ≤ p.lender.credit)
    (hpost : postSlashCredit m p.lender ≤ p.lender.credit) :
    postSlashPendingFee m p ≤ postSlashCredit m p.lender := by
  unfold postSlashPendingFee
  by_cases hcredit : p.lender.credit > 0
  · simp [hcredit]
    set post := postSlashCredit m p.lender
    set diff := p.lender.credit - post
    set feeDecrease := mulDivUp p.pendingFee diff p.lender.credit
    have hcreditPos : 0 < p.lender.credit := hcredit
    have hpost' : post ≤ p.lender.credit := by
      simpa [post] using hpost
    have hsplit : post + diff = p.lender.credit := by
      unfold diff
      omega
    have hceil :
        p.pendingFee * diff ≤ feeDecrease * p.lender.credit := by
      simpa [feeDecrease, diff] using
        le_mulDivUp_mul (a := p.pendingFee) (b := diff) (d := p.lender.credit)
          hcreditPos
    have htarget :
        p.pendingFee * p.lender.credit ≤ (post + feeDecrease) * p.lender.credit := by
      calc
        p.pendingFee * p.lender.credit
            = p.pendingFee * post + p.pendingFee * diff := by
                rw [← hsplit]
                ring
        _ ≤ p.pendingFee * post + feeDecrease * p.lender.credit :=
            Nat.add_le_add_left hceil _
        _ ≤ p.lender.credit * post + feeDecrease * p.lender.credit := by
            exact Nat.add_le_add_right (Nat.mul_le_mul_right post hpending) _
        _ = (post + feeDecrease) * p.lender.credit := by ring
    have hle : p.pendingFee ≤ post + feeDecrease :=
      Nat.le_of_mul_le_mul_right htarget hcreditPos
    omega
  · have hcreditZero : p.lender.credit = 0 := by omega
    simp [hcreditZero]

theorem accruedFeeView_le_postSlashPendingFee
    (m : Market) (p : LenderPosition) (accrualEnd maturity : Nat)
    (haccrualEnd : accrualEnd ≤ maturity) :
    accruedFeeView m p accrualEnd maturity ≤ postSlashPendingFee m p := by
  unfold accruedFeeView
  by_cases hlast : p.lastAccrual < maturity
  · simp [hlast]
    apply mulDivDown_le_left
    omega
  · simp [hlast]

theorem accruedFeeView_le_postSlashCredit
    (m : Market) (p : LenderPosition) (accrualEnd maturity : Nat)
    (hpending : p.pendingFee ≤ p.lender.credit)
    (hpost : postSlashCredit m p.lender ≤ p.lender.credit)
    (haccrualEnd : accrualEnd ≤ maturity) :
    accruedFeeView m p accrualEnd maturity ≤ postSlashCredit m p.lender :=
  le_trans
    (accruedFeeView_le_postSlashPendingFee m p accrualEnd maturity haccrualEnd)
    (postSlashPendingFee_le_postSlashCredit m p hpending hpost)

theorem postUpdateCreditView_subtractionSafe
    (m : Market) (p : LenderPosition) (accrualEnd maturity : Nat)
    (hpending : p.pendingFee ≤ p.lender.credit)
    (hpost : postSlashCredit m p.lender ≤ p.lender.credit)
    (haccrualEnd : accrualEnd ≤ maturity) :
    accruedFeeView m p accrualEnd maturity ≤ postSlashCredit m p.lender ∧
      postUpdateCreditView m p accrualEnd maturity =
        postSlashCredit m p.lender - accruedFeeView m p accrualEnd maturity :=
  ⟨accruedFeeView_le_postSlashCredit m p accrualEnd maturity
      hpending hpost haccrualEnd,
    rfl⟩

theorem postUpdateCreditView_subtractionSafe_of_lossFactor
    (m : Market) (p : LenderPosition) (accrualEnd maturity : Nat)
    (hpending : p.pendingFee ≤ p.lender.credit)
    (hadvanced : p.lender.lastLossFactor ≤ m.lossFactor)
    (hm : m.lossFactor ≤ UINT128_MAX)
    (haccrualEnd : accrualEnd ≤ maturity) :
    accruedFeeView m p accrualEnd maturity ≤ postSlashCredit m p.lender ∧
      postUpdateCreditView m p accrualEnd maturity =
        postSlashCredit m p.lender - accruedFeeView m p accrualEnd maturity :=
  postUpdateCreditView_subtractionSafe m p accrualEnd maturity
    hpending (postSlashCredit_le_credit m p.lender hadvanced hm) haccrualEnd

theorem updateViewSubtractionsSafe_of_lossFactor
    (m : Market) (positions : List LenderPosition) (accrualEnd maturity : Nat)
    (hdomain : updateViewDomain m positions)
    (hm : m.lossFactor ≤ UINT128_MAX)
    (haccrualEnd : accrualEnd ≤ maturity) :
    updateViewSubtractionsSafe m accrualEnd maturity positions := by
  induction positions with
  | nil => simp [updateViewSubtractionsSafe]
  | cons p ps ih =>
      rcases hdomain with ⟨hpending, hadvanced, hdomainTail⟩
      simp [updateViewSubtractionsSafe]
      exact ⟨
        (postUpdateCreditView_subtractionSafe_of_lossFactor m p accrualEnd maturity
          hpending hadvanced hm haccrualEnd).1,
        ih hdomainTail⟩

theorem sumPostUpdateCreditView_le_sumPostSlashCredit
    (m : Market) (positions : List LenderPosition) (accrualEnd maturity : Nat) :
    sumPostUpdateCreditView m accrualEnd maturity positions ≤
      sumPostSlashCredit m (lendersOfPositions positions) := by
  induction positions with
  | nil => simp [sumPostUpdateCreditView, sumPostSlashCredit, lendersOfPositions]
  | cons p rest ih =>
      simp [sumPostUpdateCreditView, sumPostSlashCredit, lendersOfPositions]
      exact Nat.add_le_add
        (postUpdateCreditView_le_postSlashCredit m p accrualEnd maturity) ih

/-- The market-level solvency/accounting invariant requested by the audit note. -/
def totalUnitsCoversCredits (m : Market) (lenders : List Lender) : Prop :=
  sumPostSlashCredit m lenders ≤ m.totalUnits

/-- Updating a lender to the current loss factor stores exactly the up-to-date
    slashed credit, so the view value is unchanged by synchronization. -/
def updateLender (m : Market) (l : Lender) : Lender :=
  { credit := postSlashCredit m l, lastLossFactor := m.lossFactor }

theorem postSlashCredit_synced (m : Market) (l : Lender)
    (hm : m.lossFactor ≤ UINT128_MAX) :
    postSlashCredit m (updateLender m l) = postSlashCredit m l := by
  unfold updateLender postSlashCredit
  by_cases hmax : m.lossFactor = UINT128_MAX
  · by_cases hlast : l.lastLossFactor < UINT128_MAX
    · simp [hmax, hlast, mulDivDown]
    · simp [hmax, hlast]
  · have hlt : m.lossFactor < UINT128_MAX := lt_of_le_of_ne hm hmax
    by_cases hlast : l.lastLossFactor < UINT128_MAX
    · simp [hlt, hlast, mulDivDown, Nat.mul_comm]
    · simp [hlt, hlast, mulDivDown]

theorem sumPostSlashCredit_map_update (m : Market) (lenders : List Lender)
    (hm : m.lossFactor ≤ UINT128_MAX) :
    sumPostSlashCredit m (lenders.map (updateLender m)) =
      sumPostSlashCredit m lenders := by
  induction lenders with
  | nil => simp [sumPostSlashCredit]
  | cons l ls ih =>
      simp [sumPostSlashCredit, postSlashCredit_synced m l hm, ih]

/-- Synchronizing all lenders preserves the cover property because it does not
    change their up-to-date credit. -/
theorem totalUnits_cover_preserved_by_lender_updates
    (m : Market) (lenders : List Lender)
    (hm : m.lossFactor ≤ UINT128_MAX)
    (hcover : totalUnitsCoversCredits m lenders) :
    totalUnitsCoversCredits m (lenders.map (updateLender m)) := by
  unfold totalUnitsCoversCredits at *
  rwa [sumPostSlashCredit_map_update m lenders hm]

/-- Sum of stored lender credit. -/
def sumCredit : List Lender → Nat
  | [] => 0
  | l :: ls => l.credit + sumCredit ls

def syncedAt (m : Market) : List Lender → Prop
  | [] => True
  | l :: ls => l.lastLossFactor = m.lossFactor ∧ syncedAt m ls

theorem syncedAt_map_update (m : Market) (lenders : List Lender) :
    syncedAt m (lenders.map (updateLender m)) := by
  induction lenders with
  | nil => simp [syncedAt]
  | cons l ls ih =>
      simp [syncedAt, updateLender, ih]

/-- Slash a lender's already up-to-date credit by a common market ratio. This is
    the list-level analogue of the loss-factor ratio in `updatePositionView`. -/
def slashSyncedLender (numerator denominator : Nat) (l : Lender) : Lender :=
  { credit := mulDivDown l.credit numerator denominator, lastLossFactor := l.lastLossFactor }

def slashSyncedLenders (numerator denominator : Nat) : List Lender → List Lender
  | [] => []
  | l :: ls => slashSyncedLender numerator denominator l ::
      slashSyncedLenders numerator denominator ls

theorem sumCredit_slash_le_mulDivDown_sumCredit
    (lenders : List Lender) (numerator denominator : Nat) :
    sumCredit (slashSyncedLenders numerator denominator lenders) ≤
      mulDivDown (sumCredit lenders) numerator denominator := by
  induction lenders with
  | nil => simp [sumCredit, slashSyncedLenders, mulDivDown]
  | cons l ls ih =>
      calc
        sumCredit (slashSyncedLenders numerator denominator (l :: ls))
            = mulDivDown l.credit numerator denominator
                + sumCredit (slashSyncedLenders numerator denominator ls) := by
                  simp [sumCredit, slashSyncedLenders, slashSyncedLender]
        _ ≤ mulDivDown l.credit numerator denominator
              + mulDivDown (sumCredit ls) numerator denominator := by
                exact Nat.add_le_add_left ih _
        _ ≤ mulDivDown (l.credit + sumCredit ls) numerator denominator := by
              unfold mulDivDown
              simpa [Nat.add_mul] using
                Nat.add_div_le_add_div (l.credit * numerator) (sumCredit ls * numerator) denominator
        _ = mulDivDown (sumCredit (l :: ls)) numerator denominator := by
              simp [sumCredit]

/-- Concrete proportional socialization model for the cover property. If the
    market's old up-to-date lender credits were covered by `totalUnits`, and both
    total units and lender credits are slashed by the same ratio, the cover
    property is preserved. -/
theorem totalUnits_cover_preserved_by_proportional_slash
    (m : Market) (lenders : List Lender) (numerator denominator : Nat)
    (hcover : sumCredit lenders ≤ m.totalUnits) :
    sumCredit (slashSyncedLenders numerator denominator lenders) ≤
      mulDivDown m.totalUnits numerator denominator := by
  calc
    sumCredit (slashSyncedLenders numerator denominator lenders)
        ≤ mulDivDown (sumCredit lenders) numerator denominator :=
          sumCredit_slash_le_mulDivDown_sumCredit lenders numerator denominator
    _ ≤ mulDivDown m.totalUnits numerator denominator :=
          mulDivDown_mono_left hcover

theorem sumPostSlashCredit_eq_sumCredit_slash_of_synced
    (oldM newM : Market) (lenders : List Lender)
    (hold : oldM.lossFactor < UINT128_MAX)
    (hsynced : syncedAt oldM lenders) :
    sumPostSlashCredit newM lenders =
      sumCredit
        (slashSyncedLenders (UINT128_MAX - newM.lossFactor)
          (UINT128_MAX - oldM.lossFactor) lenders) := by
  induction lenders with
  | nil => simp [sumPostSlashCredit, sumCredit, slashSyncedLenders]
  | cons l ls ih =>
      rcases hsynced with ⟨hl, hls⟩
      simp [sumPostSlashCredit, sumCredit, slashSyncedLenders, slashSyncedLender,
        postSlashCredit, hl, hold, ih hls]

/--
  Loss-factor-step cover theorem.

  This is the accounting theorem closest to Midnight's storage model: if lender
  positions are synced to the old market loss factor, and the new market's
  `totalUnits` is the old `totalUnits` multiplied by the same loss-factor ratio
  that `updatePositionView` applies to lender credit, then the sum of all
  up-to-date post-slash credits is covered by the new `totalUnits`.
-/
theorem totalUnits_cover_after_lossFactor_step
    (oldM newM : Market) (lenders : List Lender)
    (hold : oldM.lossFactor < UINT128_MAX)
    (hsynced : syncedAt oldM lenders)
    (hcover : sumCredit lenders ≤ oldM.totalUnits)
    (htotal :
        mulDivDown oldM.totalUnits (UINT128_MAX - newM.lossFactor)
          (UINT128_MAX - oldM.lossFactor) ≤ newM.totalUnits) :
    totalUnitsCoversCredits newM lenders := by
  unfold totalUnitsCoversCredits
  rw [sumPostSlashCredit_eq_sumCredit_slash_of_synced oldM newM lenders hold hsynced]
  exact le_trans
    (totalUnits_cover_preserved_by_proportional_slash oldM lenders
      (UINT128_MAX - newM.lossFactor) (UINT128_MAX - oldM.lossFactor) hcover)
    htotal

/-- Solidity's bad-debt loss-factor update, abstracted to the relevant fields:

```solidity
lossFactor = MAX - (MAX - oldLossFactor).mulDivDown(totalUnits - badDebt, totalUnits)
totalUnits -= badDebt
```
-/
def lossFactorAfterBadDebt (oldTotalUnits badDebt oldLossFactor : Nat) : Nat :=
  UINT128_MAX -
    mulDivDown (UINT128_MAX - oldLossFactor) (oldTotalUnits - badDebt) oldTotalUnits

theorem lossFactorAfterBadDebt_le_max
    (oldTotalUnits badDebt oldLossFactor : Nat) :
    lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor ≤ UINT128_MAX := by
  unfold lossFactorAfterBadDebt
  exact Nat.sub_le _ _

theorem lossFactorAfterBadDebt_zero
    (oldTotalUnits oldLossFactor : Nat)
    (htotal : oldTotalUnits > 0)
    (hold : oldLossFactor ≤ UINT128_MAX) :
    lossFactorAfterBadDebt oldTotalUnits 0 oldLossFactor = oldLossFactor := by
  unfold lossFactorAfterBadDebt
  rw [Nat.sub_zero]
  rw [mulDivDown_same_den (UINT128_MAX - oldLossFactor) oldTotalUnits htotal]
  omega

theorem lossFactorAfterBadDebt_ge_old
    (oldTotalUnits badDebt oldLossFactor : Nat)
    (hold : oldLossFactor ≤ UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits) :
    oldLossFactor ≤ lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor := by
  unfold lossFactorAfterBadDebt
  have hratio :
      mulDivDown (UINT128_MAX - oldLossFactor) (oldTotalUnits - badDebt) oldTotalUnits ≤
        UINT128_MAX - oldLossFactor := by
    apply mulDivDown_le_left
    omega
  omega

def continuousFeeCreditAfterBadDebt
    (continuousFeeCredit oldLossFactor newLossFactor : Nat) : Nat :=
  if oldLossFactor < UINT128_MAX then
    mulDivDown continuousFeeCredit (UINT128_MAX - newLossFactor)
      (UINT128_MAX - oldLossFactor)
  else
    0

theorem continuousFeeCreditAfterBadDebt_noop
    (continuousFeeCredit oldLossFactor : Nat)
    (hold : oldLossFactor < UINT128_MAX) :
    continuousFeeCreditAfterBadDebt continuousFeeCredit oldLossFactor oldLossFactor =
      continuousFeeCredit := by
  unfold continuousFeeCreditAfterBadDebt
  simp [hold]
  exact mulDivDown_same_den continuousFeeCredit
    (UINT128_MAX - oldLossFactor) (by omega)

theorem continuousFeeCreditAfterBadDebt_le_current
    (continuousFeeCredit oldLossFactor newLossFactor : Nat)
    (hmono : newLossFactor ≥ oldLossFactor) :
    continuousFeeCreditAfterBadDebt continuousFeeCredit oldLossFactor newLossFactor ≤
      continuousFeeCredit := by
  unfold continuousFeeCreditAfterBadDebt
  by_cases hold : oldLossFactor < UINT128_MAX
  · simp [hold]
    apply mulDivDown_le_left
    omega
  · simp [hold]

theorem continuousFeeCreditAfterActualBadDebt_le_current
    (continuousFeeCredit oldTotalUnits badDebt oldLossFactor : Nat)
    (hbad : badDebt ≤ oldTotalUnits) :
    continuousFeeCreditAfterBadDebt continuousFeeCredit oldLossFactor
        (lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor) ≤
      continuousFeeCredit :=
  by
    by_cases hold : oldLossFactor < UINT128_MAX
    · exact continuousFeeCreditAfterBadDebt_le_current continuousFeeCredit oldLossFactor
        (lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor)
        (lossFactorAfterBadDebt_ge_old oldTotalUnits badDebt oldLossFactor
          (le_of_lt hold) hbad)
    · unfold continuousFeeCreditAfterBadDebt
      simp [hold]

theorem lossFactor_badDebt_totalUnits_ratio_le
    (oldTotalUnits badDebt oldLossFactor : Nat)
    (hold : oldLossFactor < UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits) :
    mulDivDown oldTotalUnits
      (UINT128_MAX - lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor)
      (UINT128_MAX - oldLossFactor) ≤ oldTotalUnits - badDebt := by
  set newTotal := oldTotalUnits - badDebt
  set denom := UINT128_MAX - oldLossFactor
  set ratio := mulDivDown denom newTotal oldTotalUnits
  have hdenpos : 0 < denom := by
    unfold denom
    omega
  have hratio_le_denom : ratio ≤ denom := by
    unfold ratio
    exact mulDivDown_le_left (by omega)
  have hloss :
      UINT128_MAX - lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor = ratio := by
    unfold lossFactorAfterBadDebt ratio denom newTotal
    have hratio_max :
        mulDivDown (UINT128_MAX - oldLossFactor) (oldTotalUnits - badDebt) oldTotalUnits
          ≤ UINT128_MAX := by
      exact le_trans hratio_le_denom (by omega)
    omega
  rw [hloss]
  by_cases htotal : oldTotalUnits = 0
  · simp [mulDivDown, htotal] at hbad ⊢
  · have htotalpos : 0 < oldTotalUnits := Nat.pos_of_ne_zero htotal
    have hratio_mul :
        ratio * oldTotalUnits ≤ denom * newTotal := by
      unfold ratio mulDivDown
      exact Nat.div_mul_le_self _ _
    calc oldTotalUnits * ratio / denom
        ≤ newTotal * denom / denom := by
            exact Nat.div_le_div_right (by
              rw [Nat.mul_comm oldTotalUnits ratio, Nat.mul_comm newTotal denom]
              exact hratio_mul)
      _ = newTotal := by
            rw [Nat.mul_comm newTotal denom, Nat.mul_div_right newTotal hdenpos]

/--
  End-to-end accounting corollary for Midnight's bad-debt branch. Starting from a
  market whose lenders are synced to the old loss factor and whose synced credits
  are covered by `totalUnits`, the exact Solidity bad-debt update for
  `lossFactor` and `totalUnits` leaves the sum of lenders' up-to-date post-slash
  credit covered by the new `totalUnits`.
-/
theorem totalUnits_cover_after_badDebt_branch
    (oldTotalUnits badDebt oldLossFactor : Nat) (lenders : List Lender)
    (hold : oldLossFactor < UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits)
    (hsynced : syncedAt { totalUnits := oldTotalUnits, lossFactor := oldLossFactor } lenders)
    (hcover : sumCredit lenders ≤ oldTotalUnits) :
    totalUnitsCoversCredits
      { totalUnits := oldTotalUnits - badDebt,
        lossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor }
      lenders := by
  apply totalUnits_cover_after_lossFactor_step
      { totalUnits := oldTotalUnits, lossFactor := oldLossFactor }
      { totalUnits := oldTotalUnits - badDebt,
        lossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor }
      lenders hold hsynced hcover
  exact lossFactor_badDebt_totalUnits_ratio_le oldTotalUnits badDebt oldLossFactor hold hbad

theorem badDebt_covers_two_synced_postSlashCredits
    (oldTotalUnits badDebt oldLossFactor credit0 credit1 : Nat)
    (hold : oldLossFactor < UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits)
    (hcover : credit0 + credit1 ≤ oldTotalUnits) :
    sumPostSlashCredit
        { totalUnits := oldTotalUnits - badDebt,
          lossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor }
        [{ credit := credit0, lastLossFactor := oldLossFactor },
         { credit := credit1, lastLossFactor := oldLossFactor }] ≤
      oldTotalUnits - badDebt := by
  have hcover' :
      sumCredit
        [{ credit := credit0, lastLossFactor := oldLossFactor },
         { credit := credit1, lastLossFactor := oldLossFactor }] ≤ oldTotalUnits := by
    simpa [sumCredit] using hcover
  have hsynced :
      syncedAt { totalUnits := oldTotalUnits, lossFactor := oldLossFactor }
        [{ credit := credit0, lastLossFactor := oldLossFactor },
         { credit := credit1, lastLossFactor := oldLossFactor }] := by
    simp [syncedAt]
  have h := totalUnits_cover_after_badDebt_branch oldTotalUnits badDebt oldLossFactor
    [{ credit := credit0, lastLossFactor := oldLossFactor },
     { credit := credit1, lastLossFactor := oldLossFactor }]
    hold hbad hsynced hcover'
  simpa [totalUnitsCoversCredits] using h

def badDebtCoversTwoPostSlashCredits
    (oldTotalUnits badDebt oldLossFactor credit0 credit1 : Nat) : Prop :=
  let newLossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor
  let newTotalUnits := oldTotalUnits - badDebt
  let postSlashCredit0 :=
    mulDivDown credit0 (UINT128_MAX - newLossFactor) (UINT128_MAX - oldLossFactor)
  let postSlashCredit1 :=
    mulDivDown credit1 (UINT128_MAX - newLossFactor) (UINT128_MAX - oldLossFactor)
  postSlashCredit0 + postSlashCredit1 ≤ newTotalUnits

theorem badDebtCoversTwoPostSlashCredits_of_synced_cover
    (oldTotalUnits badDebt oldLossFactor credit0 credit1 : Nat)
    (hold : oldLossFactor < UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits)
    (hcover : credit0 + credit1 ≤ oldTotalUnits) :
    badDebtCoversTwoPostSlashCredits oldTotalUnits badDebt oldLossFactor credit0 credit1 := by
  unfold badDebtCoversTwoPostSlashCredits
  have h := badDebt_covers_two_synced_postSlashCredits oldTotalUnits badDebt
    oldLossFactor credit0 credit1 hold hbad hcover
  simpa [sumPostSlashCredit, postSlashCredit, hold] using h

def badDebtCoversTwoPostUpdateCredits
    (oldTotalUnits badDebt oldLossFactor credit0 accruedFee0 credit1 accruedFee1 : Nat) :
    Prop :=
  let newLossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor
  let newTotalUnits := oldTotalUnits - badDebt
  let postSlashCredit0 :=
    mulDivDown credit0 (UINT128_MAX - newLossFactor) (UINT128_MAX - oldLossFactor)
  let postSlashCredit1 :=
    mulDivDown credit1 (UINT128_MAX - newLossFactor) (UINT128_MAX - oldLossFactor)
  let postUpdateCredit0 := postSlashCredit0 - accruedFee0
  let postUpdateCredit1 := postSlashCredit1 - accruedFee1
  postUpdateCredit0 + postUpdateCredit1 ≤ newTotalUnits

theorem badDebtCoversTwoPostUpdateCredits_of_synced_cover
    (oldTotalUnits badDebt oldLossFactor credit0 accruedFee0 credit1 accruedFee1 : Nat)
    (hold : oldLossFactor < UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits)
    (hcover : credit0 + credit1 ≤ oldTotalUnits) :
    badDebtCoversTwoPostUpdateCredits oldTotalUnits badDebt oldLossFactor
      credit0 accruedFee0 credit1 accruedFee1 := by
  unfold badDebtCoversTwoPostUpdateCredits
  have hpost := badDebtCoversTwoPostSlashCredits_of_synced_cover oldTotalUnits badDebt
    oldLossFactor credit0 credit1 hold hbad hcover
  unfold badDebtCoversTwoPostSlashCredits at hpost
  exact le_trans (Nat.add_le_add (Nat.sub_le _ _) (Nat.sub_le _ _)) hpost

theorem sumCredit_map_update_eq_sumPostSlashCredit
    (m : Market) (lenders : List Lender) (hm : m.lossFactor ≤ UINT128_MAX) :
    sumCredit (lenders.map (updateLender m)) =
      sumPostSlashCredit m (lenders.map (updateLender m)) := by
  induction lenders with
  | nil => simp [sumCredit, sumPostSlashCredit]
  | cons l ls ih =>
      change postSlashCredit m l + sumCredit (ls.map (updateLender m)) =
        postSlashCredit m (updateLender m l) +
          sumPostSlashCredit m (ls.map (updateLender m))
      rw [postSlashCredit_synced m l hm, ih]

/-- After slashing and after every lender has been brought up to date, the stored
    credits themselves are covered by `totalUnits`. This is the form closest to
    "the sum of all the credit of lenders after slashing". -/
theorem totalUnits_covers_synced_credit_after_slashing
    (m' : Market) (lenders : List Lender)
    (hm : m'.lossFactor ≤ UINT128_MAX)
    (hcover : totalUnitsCoversCredits m' lenders) :
    sumCredit (lenders.map (updateLender m')) ≤ m'.totalUnits := by
  have hcover' := totalUnits_cover_preserved_by_lender_updates m' lenders hm hcover
  unfold totalUnitsCoversCredits at hcover'
  have hsum :
      sumCredit (lenders.map (updateLender m')) =
        sumPostSlashCredit m' (lenders.map (updateLender m')) :=
    sumCredit_map_update_eq_sumPostSlashCredit m' lenders hm
  rwa [hsum]

theorem badDebt_covers_two_storedCreditsAfterUpdates
    (oldTotalUnits badDebt oldLossFactor credit0 credit1 : Nat)
    (hold : oldLossFactor < UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits)
    (hcover : credit0 + credit1 ≤ oldTotalUnits) :
    sumCredit
        ([{ credit := credit0, lastLossFactor := oldLossFactor },
          { credit := credit1, lastLossFactor := oldLossFactor }].map
          (updateLender
            { totalUnits := oldTotalUnits - badDebt,
              lossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor })) ≤
      oldTotalUnits - badDebt := by
  have hpost :
      totalUnitsCoversCredits
        { totalUnits := oldTotalUnits - badDebt,
          lossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor }
        [{ credit := credit0, lastLossFactor := oldLossFactor },
         { credit := credit1, lastLossFactor := oldLossFactor }] := by
    unfold totalUnitsCoversCredits
    exact badDebt_covers_two_synced_postSlashCredits oldTotalUnits badDebt
      oldLossFactor credit0 credit1 hold hbad hcover
  exact totalUnits_covers_synced_credit_after_slashing
    { totalUnits := oldTotalUnits - badDebt,
      lossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor }
    [{ credit := credit0, lastLossFactor := oldLossFactor },
     { credit := credit1, lastLossFactor := oldLossFactor }]
    (lossFactorAfterBadDebt_le_max oldTotalUnits badDebt oldLossFactor)
    hpost

def badDebtCoversTwoStoredCreditsAfterUpdates
    (oldTotalUnits badDebt oldLossFactor credit0 credit1 : Nat) : Prop :=
  let newLossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor
  let newTotalUnits := oldTotalUnits - badDebt
  let storedCredit0 :=
    mulDivDown credit0 (UINT128_MAX - newLossFactor) (UINT128_MAX - oldLossFactor)
  let storedCredit1 :=
    mulDivDown credit1 (UINT128_MAX - newLossFactor) (UINT128_MAX - oldLossFactor)
  storedCredit0 + storedCredit1 ≤ newTotalUnits

theorem badDebtCoversTwoStoredCreditsAfterUpdates_of_synced_cover
    (oldTotalUnits badDebt oldLossFactor credit0 credit1 : Nat)
    (hold : oldLossFactor < UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits)
    (hcover : credit0 + credit1 ≤ oldTotalUnits) :
    badDebtCoversTwoStoredCreditsAfterUpdates oldTotalUnits badDebt oldLossFactor
      credit0 credit1 := by
  unfold badDebtCoversTwoStoredCreditsAfterUpdates
  have h := badDebt_covers_two_storedCreditsAfterUpdates oldTotalUnits badDebt
    oldLossFactor credit0 credit1 hold hbad hcover
  simpa [sumCredit, updateLender, postSlashCredit, hold] using h

/-- Since `updatePositionView` subtracts accrued continuous fee from post-slash
    credit, any cover theorem for post-slash credit also covers the actual
    returned up-to-date credit values. -/
theorem totalUnits_covers_postUpdateCredit_after_slashing
    (m' : Market) (positions : List LenderPosition) (accrualEnd maturity : Nat)
    (hcover : totalUnitsCoversCredits m' (lendersOfPositions positions)) :
    sumPostUpdateCreditView m' accrualEnd maturity positions ≤ m'.totalUnits := by
  exact le_trans
    (sumPostUpdateCreditView_le_sumPostSlashCredit m' positions accrualEnd maturity) hcover

theorem totalUnits_covers_updatePositionViewResultCredit_after_slashing
    (m' : Market) (positions : List LenderPosition) (timestamp maturity : Nat)
    (hcover : totalUnitsCoversCredits m' (lendersOfPositions positions)) :
    sumUpdatePositionViewResultCredit m' timestamp maturity positions ≤ m'.totalUnits := by
  rw [sumUpdatePositionViewResultCredit_eq_sumPostUpdateCreditView]
  exact totalUnits_covers_postUpdateCredit_after_slashing m' positions
    (accrualEnd timestamp maturity) maturity hcover

theorem badDebt_covers_two_postUpdateCredits
    (oldTotalUnits badDebt oldLossFactor credit0 credit1 pendingFee0 pendingFee1
      lastAccrual0 lastAccrual1 accrualEnd maturity : Nat)
    (hold : oldLossFactor < UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits)
    (hcover : credit0 + credit1 ≤ oldTotalUnits) :
    sumPostUpdateCreditView
        { totalUnits := oldTotalUnits - badDebt,
          lossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor }
        accrualEnd maturity
        [{ lender := { credit := credit0, lastLossFactor := oldLossFactor },
           pendingFee := pendingFee0,
           lastAccrual := lastAccrual0 },
         { lender := { credit := credit1, lastLossFactor := oldLossFactor },
           pendingFee := pendingFee1,
           lastAccrual := lastAccrual1 }] ≤
      oldTotalUnits - badDebt := by
  apply totalUnits_covers_postUpdateCredit_after_slashing
  unfold lendersOfPositions
  unfold totalUnitsCoversCredits
  exact badDebt_covers_two_synced_postSlashCredits oldTotalUnits badDebt
    oldLossFactor credit0 credit1 hold hbad hcover

theorem badDebt_covers_two_updatePositionViewResultCredits
    (oldTotalUnits badDebt oldLossFactor credit0 credit1 pendingFee0 pendingFee1
      lastAccrual0 lastAccrual1 timestamp maturity : Nat)
    (hold : oldLossFactor < UINT128_MAX)
    (hbad : badDebt ≤ oldTotalUnits)
    (hcover : credit0 + credit1 ≤ oldTotalUnits) :
    sumUpdatePositionViewResultCredit
        { totalUnits := oldTotalUnits - badDebt,
          lossFactor := lossFactorAfterBadDebt oldTotalUnits badDebt oldLossFactor }
        timestamp maturity
        [{ lender := { credit := credit0, lastLossFactor := oldLossFactor },
           pendingFee := pendingFee0,
           lastAccrual := lastAccrual0 },
         { lender := { credit := credit1, lastLossFactor := oldLossFactor },
           pendingFee := pendingFee1,
           lastAccrual := lastAccrual1 }] ≤
      oldTotalUnits - badDebt := by
  rw [sumUpdatePositionViewResultCredit_eq_sumPostUpdateCreditView]
  exact badDebt_covers_two_postUpdateCredits oldTotalUnits badDebt oldLossFactor
    credit0 credit1 pendingFee0 pendingFee1 lastAccrual0 lastAccrual1
    (accrualEnd timestamp maturity) maturity hold hbad hcover

end Midnight.Proofs.UnitsAccounting
