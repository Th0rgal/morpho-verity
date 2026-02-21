/-
  Nat-level list sum lemmas for share accounting consistency proofs.

  The share accounting specs use `.val` (Nat) rather than `Uint256` to avoid
  wrapping in the sum. These two lemmas are the workhorses: they show how
  `List.map ... .sum` changes when exactly one element is incremented or
  decremented.
-/

private theorem map_eq_map_of_not_mem [DecidableEq α]
    {l : List α} {f g : α → Nat} {target : α}
    (h_not_mem : target ∉ l)
    (h_eq : ∀ a, a ≠ target → g a = f a) :
    l.map g = l.map f := by
  apply List.map_congr_left
  intro a h_a
  exact h_eq a (fun h => h_not_mem (h ▸ h_a))

theorem le_sum_of_mem {l : List Nat} (h : n ∈ l) : n ≤ l.sum := by
  induction l with
  | nil => exact absurd h (List.not_mem_nil _)
  | cons x xs ih =>
    simp only [List.sum_cons]
    rcases List.mem_cons.mp h with rfl | h_xs
    · exact Nat.le_add_right _ _
    · exact Nat.le_trans (ih h_xs) (Nat.le_add_left _ _)

theorem list_sum_map_add [DecidableEq α]
    (l : List α) (f : α → Nat) (target : α) (delta : Nat)
    (h_mem : target ∈ l) (h_nodup : l.Nodup) :
    (l.map (fun a => if a = target then f target + delta else f a)).sum
    = (l.map f).sum + delta := by
  induction l with
  | nil => exact absurd h_mem (List.not_mem_nil _)
  | cons x xs ih =>
    simp only [List.map, List.sum_cons]
    have ⟨h_not_mem, h_nodup_xs⟩ := List.nodup_cons.mp h_nodup
    rcases List.mem_cons.mp h_mem with rfl | h_mem_xs
    · -- x = target: the head changes, tail is unchanged
      simp only [ite_true]
      have h_tail : xs.map (fun a => if a = target then f target + delta else f a) = xs.map f :=
        map_eq_map_of_not_mem h_not_mem (fun a h_ne => by simp [h_ne])
      rw [h_tail]; omega
    · -- x ≠ target: head unchanged, recurse on tail
      have h_ne : x ≠ target := fun h => h_not_mem (h ▸ h_mem_xs)
      simp only [h_ne, ite_false]
      rw [ih h_mem_xs h_nodup_xs]; omega

theorem list_sum_map_zero [DecidableEq α]
    (l : List α) (f : α → Nat) (target : α)
    (h_mem : target ∈ l) (h_nodup : l.Nodup) :
    (l.map (fun a => if a = target then 0 else f a)).sum
    = (l.map f).sum - f target := by
  induction l with
  | nil => exact absurd h_mem (List.not_mem_nil _)
  | cons x xs ih =>
    simp only [List.map, List.sum_cons]
    have ⟨h_not_mem, h_nodup_xs⟩ := List.nodup_cons.mp h_nodup
    rcases List.mem_cons.mp h_mem with rfl | h_mem_xs
    · -- x = target: the head is zeroed, tail is unchanged
      simp only [ite_true]
      have h_tail : xs.map (fun a => if a = target then 0 else f a) = xs.map f :=
        map_eq_map_of_not_mem h_not_mem (fun a h_ne => by simp [h_ne])
      rw [h_tail]; omega
    · -- x ≠ target: head unchanged, recurse on tail
      have h_ne : x ≠ target := fun h => h_not_mem (h ▸ h_mem_xs)
      simp only [h_ne, ite_false]
      have h_ft_le_sum : f target ≤ (xs.map f).sum :=
        le_sum_of_mem (List.mem_map_of_mem f h_mem_xs)
      rw [ih h_mem_xs h_nodup_xs]; omega

theorem list_sum_map_sub [DecidableEq α]
    (l : List α) (f : α → Nat) (target : α) (delta : Nat)
    (h_mem : target ∈ l) (h_nodup : l.Nodup) (h_ge : delta ≤ f target) :
    (l.map (fun a => if a = target then f target - delta else f a)).sum
    = (l.map f).sum - delta := by
  induction l with
  | nil => exact absurd h_mem (List.not_mem_nil _)
  | cons x xs ih =>
    simp only [List.map, List.sum_cons]
    have ⟨h_not_mem, h_nodup_xs⟩ := List.nodup_cons.mp h_nodup
    rcases List.mem_cons.mp h_mem with rfl | h_mem_xs
    · -- x = target: the head changes, tail is unchanged
      simp only [ite_true]
      have h_tail : xs.map (fun a => if a = target then f target - delta else f a) = xs.map f :=
        map_eq_map_of_not_mem h_not_mem (fun a h_ne => by simp [h_ne])
      rw [h_tail]; omega
    · -- x ≠ target: head unchanged, recurse on tail
      have h_ne : x ≠ target := fun h => h_not_mem (h ▸ h_mem_xs)
      simp only [h_ne, ite_false]
      have h_delta_le_sum : delta ≤ (xs.map f).sum :=
        Nat.le_trans h_ge (le_sum_of_mem (List.mem_map_of_mem f h_mem_xs))
      rw [ih h_mem_xs h_nodup_xs]; omega
