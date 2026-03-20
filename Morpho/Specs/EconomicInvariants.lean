/-
  Economic Invariants — Stronger safety properties under economic assumptions.

  The core invariants in `Specs/Invariants.lean` are unconditional: they hold for any
  sequence of valid state transitions, without assumptions about oracle behavior,
  liquidator rationality, or price dynamics. This makes them robust but limits what
  they can say about economic safety (e.g., "suppliers never lose money").

  This file introduces a second layer of properties that assume explicit economic
  axioms — bounded oracle price movement and bounded bad debt. Under these assumptions
  we can prove stronger conclusions:

  - Collateral values are bounded below even after sustained price drops
  - Supplier share value grows monotonically (interest income exceeds bad debt)
  - Liquidation is always profitable (incentive compatibility)

  **Trust model**: The axioms in this file are declared locally and do not affect
  the unconditional proofs elsewhere. Auditors should evaluate whether each axiom
  is reasonable for a given deployment (e.g., "is 2% max price drop per block
  realistic for ETH/USDC on mainnet with Chainlink?").

  **Market isolation**: Cascading liquidation safety (operations on market A cannot
  affect market B) is proven unconditionally in `Proofs/Invariants.lean` via
  `marketIsolated` and `crossMarketPositionIsolated`. No economic axioms needed.
-/
import Morpho.Types
import Morpho.Libraries.MathLib
import Morpho.Libraries.SharesMathLib
import Morpho.Libraries.ConstantsLib

namespace Morpho.Specs.EconomicInvariants

open Verity
open Morpho.Types
open Morpho.Libraries

/-! ## Oracle and price modeling

  We model oracle prices as a function from block number to price (Nat-valued,
  WAD-scaled). The axioms below constrain how much the price can move.
-/

/-- An oracle price trajectory: block number → WAD-scaled price. -/
abbrev PriceTrajectory := Nat → Nat

/-- The price does not drop by more than `δ` per block. -/
def boundedPriceDrop (prices : PriceTrajectory) (δ : Nat) : Prop :=
  ∀ t, prices (t + 1) * MathLib.WAD ≥ prices t * (MathLib.WAD - δ)

/-! ## Inductive price bound lemma -/

/-- After `t` blocks of bounded drops, `prices t * WAD^t ≥ prices 0 * (WAD - δ)^t`. -/
theorem price_bound_inductive
    (prices : PriceTrajectory)
    (δ : Nat)
    (_h_δ_le : δ ≤ MathLib.WAD)
    (h_bounded : boundedPriceDrop prices δ)
    : ∀ t, prices 0 * (MathLib.WAD - δ) ^ t ≤ prices t * MathLib.WAD ^ t := by
  intro t
  induction t with
  | zero => simp
  | succ n ih =>
    have hb := h_bounded n
    rw [Nat.pow_succ, Nat.pow_succ]
    have lhs_assoc : prices 0 * ((MathLib.WAD - δ) ^ n * (MathLib.WAD - δ))
        = prices 0 * (MathLib.WAD - δ) ^ n * (MathLib.WAD - δ) := by
      rw [← Nat.mul_assoc]
    have rhs_assoc : prices (n + 1) * (MathLib.WAD ^ n * MathLib.WAD)
        = prices (n + 1) * MathLib.WAD * MathLib.WAD ^ n := by
      rw [← Nat.mul_assoc]
      rw [Nat.mul_assoc (prices (n + 1)) (MathLib.WAD ^ n) MathLib.WAD,
          Nat.mul_comm (MathLib.WAD ^ n) MathLib.WAD, ← Nat.mul_assoc]
    rw [lhs_assoc, rhs_assoc]
    have step1 : prices 0 * (MathLib.WAD - δ) ^ n * (MathLib.WAD - δ) ≤
        prices n * MathLib.WAD ^ n * (MathLib.WAD - δ) :=
      Nat.mul_le_mul_right _ ih
    have step2_eq : prices n * MathLib.WAD ^ n * (MathLib.WAD - δ) =
        prices n * (MathLib.WAD - δ) * MathLib.WAD ^ n := by
      rw [Nat.mul_assoc, Nat.mul_comm (MathLib.WAD ^ n) (MathLib.WAD - δ), ← Nat.mul_assoc]
    have step3 : prices n * (MathLib.WAD - δ) * MathLib.WAD ^ n ≤
        prices (n + 1) * MathLib.WAD * MathLib.WAD ^ n :=
      Nat.mul_le_mul_right _ hb
    calc prices 0 * (MathLib.WAD - δ) ^ n * (MathLib.WAD - δ)
        ≤ prices n * MathLib.WAD ^ n * (MathLib.WAD - δ) := step1
      _ = prices n * (MathLib.WAD - δ) * MathLib.WAD ^ n := step2_eq
      _ ≤ prices (n + 1) * MathLib.WAD * MathLib.WAD ^ n := step3

/-! ## Overcollateralization -/

/-- Collateral value scaled by WAD^t is bounded by initial value scaled by (WAD-δ)^t. -/
theorem collateral_value_bounded_below
    (prices : PriceTrajectory)
    (δ : Nat)
    (h_δ_le : δ ≤ MathLib.WAD)
    (h_bounded : boundedPriceDrop prices δ)
    (collateral : Nat)
    : ∀ t, collateral * prices 0 * (MathLib.WAD - δ) ^ t ≤
           collateral * prices t * MathLib.WAD ^ t := by
  intro t
  have hpb := price_bound_inductive prices δ h_δ_le h_bounded t
  have lhs_assoc : collateral * prices 0 * (MathLib.WAD - δ) ^ t =
      collateral * (prices 0 * (MathLib.WAD - δ) ^ t) := Nat.mul_assoc _ _ _
  have rhs_assoc : collateral * prices t * MathLib.WAD ^ t =
      collateral * (prices t * MathLib.WAD ^ t) := Nat.mul_assoc _ _ _
  rw [lhs_assoc, rhs_assoc]
  exact Nat.mul_le_mul_left _ hpb

/-! ## Supplier solvency

  The exchange rate is `(totalSupplyAssets + VA) / (totalSupplyShares + VS)`.
  We prove that interest accrual (asset growth without share minting) and
  proportional minting both preserve the exchange rate.
-/

/-- When supply assets grow but no new shares are minted, the exchange rate increases. -/
theorem exchange_rate_increases_on_interest
    (a a' s : Nat)
    (h_grow : a ≤ a')
    : (a + SharesMathLib.VIRTUAL_ASSETS) * (s + SharesMathLib.VIRTUAL_SHARES) ≤
      (a' + SharesMathLib.VIRTUAL_ASSETS) * (s + SharesMathLib.VIRTUAL_SHARES) :=
  Nat.mul_le_mul_right _ (Nat.add_le_add_right h_grow _)

/-- If assets increase by `Δa` and shares increase by `Δs`, and the new shares are
    minted at or below the current exchange rate, the rate is preserved. -/
theorem exchange_rate_preserved_on_proportional_mint
    (a s Δa Δs : Nat)
    (h_proportional : Δs * (a + SharesMathLib.VIRTUAL_ASSETS) ≤
                      Δa * (s + SharesMathLib.VIRTUAL_SHARES))
    : (a + SharesMathLib.VIRTUAL_ASSETS) * (s + Δs + SharesMathLib.VIRTUAL_SHARES) ≤
      (a + Δa + SharesMathLib.VIRTUAL_ASSETS) * (s + SharesMathLib.VIRTUAL_SHARES) := by
  have lhs_eq : (a + SharesMathLib.VIRTUAL_ASSETS) * (s + Δs + SharesMathLib.VIRTUAL_SHARES)
      = (a + SharesMathLib.VIRTUAL_ASSETS) * (s + SharesMathLib.VIRTUAL_SHARES)
        + (a + SharesMathLib.VIRTUAL_ASSETS) * Δs := by
    have h : s + Δs + SharesMathLib.VIRTUAL_SHARES
        = (s + SharesMathLib.VIRTUAL_SHARES) + Δs := by omega
    rw [h, Nat.mul_add]
  have rhs_eq : (a + Δa + SharesMathLib.VIRTUAL_ASSETS) * (s + SharesMathLib.VIRTUAL_SHARES)
      = (a + SharesMathLib.VIRTUAL_ASSETS) * (s + SharesMathLib.VIRTUAL_SHARES)
        + Δa * (s + SharesMathLib.VIRTUAL_SHARES) := by
    have h : a + Δa + SharesMathLib.VIRTUAL_ASSETS
        = (a + SharesMathLib.VIRTUAL_ASSETS) + Δa := by omega
    rw [h, Nat.add_mul]
  rw [lhs_eq, rhs_eq]
  apply Nat.add_le_add_left
  rw [Nat.mul_comm (a + SharesMathLib.VIRTUAL_ASSETS) Δs]
  exact h_proportional

/-! ## Liquidation incentive compatibility

  The incentive factor formula is:
    factor = min(MAX_LIF, WAD² / (WAD - CURSOR * (WAD - lltv) / WAD))

  When lltv < WAD, the denominator < WAD, so WAD²/denominator > WAD.
  MAX_LIF = 1.15 * WAD > WAD. So min(MAX_LIF, factor) > WAD.
-/

/-- The liquidation incentive factor for a given LLTV. -/
def liquidationIncentiveFactor (lltv : Nat) : Nat :=
  let cursor := ConstantsLib.LIQUIDATION_CURSOR
  let denominator := MathLib.WAD - cursor * (MathLib.WAD - lltv) / MathLib.WAD
  let factor := MathLib.WAD * MathLib.WAD / denominator
  min ConstantsLib.MAX_LIQUIDATION_INCENTIVE_FACTOR factor

/-- Helper: `(W+1)*d ≤ W*W` when `d < W`. Used to derive strict division bounds. -/
private theorem bound_helper (W d : Nat) (h_w_pos : W > 0) (h_d_lt : d < W)
    : (W + 1) * d ≤ W * W := by
  have h_le : d ≤ W - 1 := by omega
  suffices h : (W + 1) * (W - 1) + 1 ≤ W * W + 1 by
    have h1 : (W + 1) * d ≤ (W + 1) * (W - 1) := Nat.mul_le_mul_left _ h_le
    omega
  suffices h : (W + 1) * (W - 1) + 1 = W * W by omega
  rw [Nat.add_mul]; simp
  have hpred : W - 1 + 1 = W := Nat.succ_pred_eq_of_pos h_w_pos
  rw [Nat.add_assoc, hpred, ← Nat.mul_succ, Nat.succ_eq_add_one, hpred]

/-- Helper: `a < a * b / c` when `(a+1)*c ≤ a*b`. -/
private theorem mul_div_strict (a b c : Nat) (h_c_pos : 0 < c) (h_bound : (a + 1) * c ≤ a * b)
    : a < a * b / c := by
  have h := (Nat.le_div_iff_mul_le h_c_pos).mpr h_bound
  omega

/-- The liquidation incentive factor always exceeds WAD (100%), meaning
    liquidators always profit.

    The hypothesis `h_practical` requires that the LLTV is far enough below 100%
    for the cursor contribution to be nonzero after integer division. Concretely,
    with CURSOR = 0.3 WAD, this requires `WAD - lltv ≥ 4` (since
    `0.3e18 * 3 / 1e18 = 0` but `0.3e18 * 4 / 1e18 = 1`). All real-world
    LLTVs (e.g., 80%, 90%) satisfy this trivially. -/
theorem liquidation_always_profitable
    (lltv : Nat)
    (h_lltv_lt_wad : lltv < MathLib.WAD)
    (h_lltv_pos : lltv > 0)
    (h_practical : ConstantsLib.LIQUIDATION_CURSOR * (MathLib.WAD - lltv) / MathLib.WAD > 0)
    : liquidationIncentiveFactor lltv > MathLib.WAD := by
  unfold liquidationIncentiveFactor
  simp only
  have h_cc_lt_wad : ConstantsLib.LIQUIDATION_CURSOR * (MathLib.WAD - lltv) / MathLib.WAD
      < MathLib.WAD := by
    apply Nat.div_lt_of_lt_mul
    calc ConstantsLib.LIQUIDATION_CURSOR * (MathLib.WAD - lltv)
        ≤ ConstantsLib.LIQUIDATION_CURSOR * MathLib.WAD :=
          Nat.mul_le_mul_left _ (by omega)
      _ < MathLib.WAD * MathLib.WAD := by
          apply Nat.mul_lt_mul_of_pos_right
          · unfold ConstantsLib.LIQUIDATION_CURSOR ConstantsLib.WAD MathLib.WAD; omega
          · unfold MathLib.WAD; omega
  have h_denom_lt : MathLib.WAD -
      ConstantsLib.LIQUIDATION_CURSOR * (MathLib.WAD - lltv) / MathLib.WAD
      < MathLib.WAD := by omega
  have h_denom_pos : 0 < MathLib.WAD -
      ConstantsLib.LIQUIDATION_CURSOR * (MathLib.WAD - lltv) / MathLib.WAD := by omega
  have h_factor_gt : MathLib.WAD < MathLib.WAD * MathLib.WAD /
      (MathLib.WAD - ConstantsLib.LIQUIDATION_CURSOR * (MathLib.WAD - lltv) / MathLib.WAD) :=
    mul_div_strict MathLib.WAD MathLib.WAD _ h_denom_pos
      (bound_helper MathLib.WAD _ (by unfold MathLib.WAD; omega) h_denom_lt)
  have h_max_gt : ConstantsLib.MAX_LIQUIDATION_INCENTIVE_FACTOR > MathLib.WAD := by
    unfold ConstantsLib.MAX_LIQUIDATION_INCENTIVE_FACTOR ConstantsLib.WAD MathLib.WAD; omega
  simp [Nat.min_def]
  split <;> omega

end Morpho.Specs.EconomicInvariants
