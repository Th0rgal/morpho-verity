#!/usr/bin/env python3
"""Fail-closed tracker for macro-native migrated Morpho selector slice."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any

from check_macro_migration_surface import (  # reuse canonical spec-signature parser
  MacroMigrationSurfaceError,
  SPEC_PATH,
  extract_spec_signatures,
  read_text as read_surface_text,
)


ROOT = pathlib.Path(__file__).resolve().parent.parent
MACRO_PATH = ROOT / "Morpho" / "Compiler" / "MacroSlice.lean"
BASELINE_PATH = ROOT / "config" / "macro-migration-slice.json"
DEFAULT_CONTRACT = "MorphoViewSlice"

CONTRACT_RE = re.compile(r"^\s*verity_contract\s+([A-Za-z_][A-Za-z0-9_]*)\s+where\s*$")
FUNCTION_RE = re.compile(
  r"^\s*function\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*:\s*(.+?)\s*:=\s*"
)
PARAM_RE = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*(.+?)\s*$")

TYPE_MAP = {
  "Address": "address",
  "Uint256": "uint256",
  "Uint8": "uint8",
  "Bool": "bool",
  "Bytes32": "bytes32",
  "Bytes": "bytes",
}


class MigrationSliceError(RuntimeError):
  pass


def display_path(path: pathlib.Path) -> str:
  try:
    return str(path.relative_to(ROOT))
  except ValueError:
    return str(path)


def read_text(path: pathlib.Path) -> str:
  try:
    return read_surface_text(path)
  except MacroMigrationSurfaceError as exc:
    raise MigrationSliceError(str(exc)) from exc


def _normalize_type(type_src: str) -> str:
  return " ".join(type_src.strip().split())


def _sol_type(lean_ty: str) -> str:
  ty = _normalize_type(lean_ty)
  if ty in TYPE_MAP:
    return TYPE_MAP[ty]
  if ty.startswith("Array "):
    elem = ty[len("Array "):].strip()
    return f"{_sol_type(elem)}[]"
  if ty.startswith("Tuple [") and ty.endswith("]"):
    inner = ty[len("Tuple ["):-1]
    elems = _split_top_level(inner)
    if len(elems) < 2:
      raise MigrationSliceError(f"tuple must have at least 2 elements: {ty!r}")
    return "(" + ",".join(_sol_type(elem) for elem in elems) + ")"
  raise MigrationSliceError(f"unsupported parameter type in macro slice: {ty!r}")


def _split_top_level(src: str) -> list[str]:
  src = src.strip()
  if not src:
    return []
  out: list[str] = []
  current: list[str] = []
  depth_paren = 0
  depth_bracket = 0
  for ch in src:
    if ch == "(":
      depth_paren += 1
      current.append(ch)
    elif ch == ")":
      depth_paren -= 1
      current.append(ch)
    elif ch == "[":
      depth_bracket += 1
      current.append(ch)
    elif ch == "]":
      depth_bracket -= 1
      current.append(ch)
    elif ch == "," and depth_paren == 0 and depth_bracket == 0:
      part = "".join(current).strip()
      if part:
        out.append(part)
      current = []
    else:
      current.append(ch)
  tail = "".join(current).strip()
  if tail:
    out.append(tail)
  return out


def _split_params(params_src: str) -> list[str]:
  out: list[str] = []
  for part in _split_top_level(params_src):
    m = PARAM_RE.match(part)
    if not m:
      raise MigrationSliceError(f"invalid parameter declaration: {part!r}")
    out.append(_sol_type(m.group(2)))
  return out


def extract_macro_signatures(text: str, contract_name: str = DEFAULT_CONTRACT) -> set[str]:
  in_contract = False
  signatures: set[str] = set()

  for line in text.splitlines():
    cm = CONTRACT_RE.match(line)
    if cm:
      in_contract = cm.group(1) == contract_name
      continue
    if not in_contract:
      continue

    fm = FUNCTION_RE.match(line)
    if not fm:
      continue
    name = fm.group(1)
    params = ",".join(_split_params(fm.group(2)))
    signatures.add(f"{name}({params})")

  if not signatures:
    raise MigrationSliceError(
      f"no function signatures found for verity_contract {contract_name} in macro slice source"
    )
  return signatures


def load_baseline(path: pathlib.Path) -> dict[str, Any]:
  try:
    with path.open("r", encoding="utf-8") as f:
      data = json.load(f)
  except UnicodeDecodeError as exc:
    raise MigrationSliceError(f"baseline file is not valid UTF-8: {path}: {exc}") from exc
  except OSError as exc:
    raise MigrationSliceError(f"failed to read baseline file {path}: {exc}") from exc
  except json.JSONDecodeError as exc:
    raise MigrationSliceError(f"baseline file is not valid JSON: {path}") from exc
  if not isinstance(data, dict):
    raise MigrationSliceError("baseline file must contain a JSON object")
  return data


def write_baseline(path: pathlib.Path, data: dict[str, Any]) -> None:
  try:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as f:
      json.dump(data, f, indent=2, sort_keys=True)
      f.write("\n")
  except OSError as exc:
    raise MigrationSliceError(f"failed to write baseline file {path}: {exc}") from exc


def validate_against_baseline(actual: set[str], baseline: dict[str, Any]) -> None:
  expected_migrated = baseline.get("expectedMigrated")
  if not isinstance(expected_migrated, list):
    raise MigrationSliceError("baseline expectedMigrated must be a list of non-empty signatures")
  if any(not isinstance(signature, str) or not signature.strip() for signature in expected_migrated):
    raise MigrationSliceError("baseline expectedMigrated must contain only non-empty signatures")
  if len(expected_migrated) != len(set(expected_migrated)):
    raise MigrationSliceError("baseline expectedMigrated contains duplicate signatures")
  expected = set(expected_migrated)
  if actual != expected:
    missing = sorted(expected - actual)
    extra = sorted(actual - expected)
    raise MigrationSliceError(
      "macro migrated-signature set drift detected: "
      f"missing={missing} extra={extra}"
    )


def validate_baseline_metadata(
  baseline: dict[str, Any],
  macro_path: pathlib.Path,
  contract_name: str,
) -> None:
  baseline_contract = baseline.get("contract")
  if not isinstance(baseline_contract, str) or not baseline_contract.strip():
    raise MigrationSliceError("baseline contract must be a non-empty string")
  if baseline_contract != contract_name:
    raise MigrationSliceError(
      f"baseline contract mismatch: expected {contract_name!r}, got {baseline_contract!r}"
    )

  baseline_source = baseline.get("source")
  if not isinstance(baseline_source, str) or not baseline_source.strip():
    raise MigrationSliceError("baseline source must be a non-empty string")
  expected_source = display_path(macro_path)
  if baseline_source != expected_source:
    raise MigrationSliceError(
      f"baseline source mismatch: expected {expected_source!r}, got {baseline_source!r}"
    )


def validate_blocked_against_baseline(
  spec_signatures: set[str],
  migrated_signatures: set[str],
  baseline: dict[str, Any],
) -> dict[str, str]:
  blocked = validate_blocked_reasons(baseline.get("expectedBlocked"), context="baseline")


def validate_blocked_reasons(blocked: Any, *, context: str) -> dict[str, str]:
  if not isinstance(blocked, dict):
    raise MigrationSliceError(f"{context} expectedBlocked must be a map of signature -> reason")
  invalid_signature = [
    sig for sig in blocked
    if not isinstance(sig, str) or not sig.strip()
  ]
  if invalid_signature:
    raise MigrationSliceError(
      f"{context} expectedBlocked contains invalid signature key(s): "
      f"{sorted(repr(sig) for sig in invalid_signature)}"
    )

  invalid_reason = [sig for sig, reason in blocked.items() if not isinstance(reason, str) or not reason.strip()]
  if invalid_reason:
    raise MigrationSliceError(
      f"{context} expectedBlocked contains empty/invalid reason(s): "
      f"{sorted(invalid_reason)}"
    )
  placeholder_reason = [
    sig
    for sig, reason in blocked.items()
    if isinstance(reason, str) and reason.strip() == "pending macro migration (tracked blocker)"
  ]
  if placeholder_reason:
    raise MigrationSliceError(
      f"{context} expectedBlocked contains placeholder reason(s); use explicit blocker categories: "
      f"{sorted(placeholder_reason)}"
    )

  return {sig: str(blocked[sig]).strip() for sig in sorted(blocked)}


def validate_blocked_against_baseline(
  spec_signatures: set[str],
  migrated_signatures: set[str],
  baseline: dict[str, Any],
) -> dict[str, str]:
  blocked = validate_blocked_reasons(baseline.get("expectedBlocked"), context="baseline")

  expected_blocked = set(blocked)
  actual_blocked = spec_signatures - migrated_signatures
  overlap = expected_blocked & migrated_signatures
  if overlap:
    raise MigrationSliceError(
      "baseline expectedBlocked overlaps migrated signatures: "
      f"{sorted(overlap)}"
    )

  if expected_blocked != actual_blocked:
    missing = sorted(actual_blocked - expected_blocked)
    extra = sorted(expected_blocked - actual_blocked)
    raise MigrationSliceError(
      "macro blocked-signature set drift detected: "
      f"missing={missing} extra={extra}"
    )

  return {sig: blocked[sig] for sig in sorted(expected_blocked)}


def build_write_baseline(
  spec_signatures: set[str],
  migrated_signatures: set[str],
  *,
  macro_path: pathlib.Path,
  contract_name: str,
  existing_baseline: dict[str, Any] | None = None,
) -> dict[str, Any]:
  blocked = sorted(spec_signatures - migrated_signatures)
  blocked_reasons: dict[str, str] = {}
  if existing_baseline is not None:
    blocked_reasons = validate_blocked_reasons(
      existing_baseline.get("expectedBlocked"),
      context="existing baseline",
    )

  missing_reasons = sorted(sig for sig in blocked if sig not in blocked_reasons)
  if missing_reasons:
    raise MigrationSliceError(
      "cannot --write baseline with newly blocked signatures lacking explicit reasons: "
      f"{missing_reasons}"
    )

  return {
    "source": display_path(macro_path),
    "contract": contract_name,
    "expectedMigrated": sorted(migrated_signatures),
    "expectedBlocked": {sig: blocked_reasons[sig] for sig in blocked},
    "notes": (
      "Fail-closed macro-native migration slice tracker. "
      "Changes require explicit reviewed baseline updates; "
      "every non-migrated signature must stay explicitly classified with a blocker reason."
    ),
  }


def run_check(
  spec_path: pathlib.Path = SPEC_PATH,
  macro_path: pathlib.Path = MACRO_PATH,
  baseline_path: pathlib.Path = BASELINE_PATH,
  contract_name: str = DEFAULT_CONTRACT,
) -> dict[str, Any]:
  spec_signatures = extract_spec_signatures(read_text(spec_path))
  migrated_signatures = extract_macro_signatures(read_text(macro_path), contract_name=contract_name)
  unknown_in_spec = sorted(migrated_signatures - spec_signatures)
  if unknown_in_spec:
    raise MigrationSliceError(
      "migrated macro signatures are not present in morphoSpec selector surface: "
      f"{unknown_in_spec}"
    )

  baseline = load_baseline(baseline_path)
  validate_baseline_metadata(
    baseline=baseline,
    macro_path=macro_path,
    contract_name=contract_name,
  )
  validate_against_baseline(migrated_signatures, baseline)
  blocked_signatures = validate_blocked_against_baseline(
    spec_signatures=spec_signatures,
    migrated_signatures=migrated_signatures,
    baseline=baseline,
  )

  coverage = round(100.0 * len(migrated_signatures) / max(len(spec_signatures), 1), 2)
  return {
    "status": "ok",
    "specPath": display_path(spec_path),
    "macroPath": display_path(macro_path),
    "baselinePath": display_path(baseline_path),
    "contract": contract_name,
    "migratedCount": len(migrated_signatures),
    "specSignatureCount": len(spec_signatures),
    "coveragePct": coverage,
    "migratedSignatures": sorted(migrated_signatures),
    "blockedCount": len(blocked_signatures),
    "blockedSignatures": blocked_signatures,
  }


def parser() -> argparse.ArgumentParser:
  p = argparse.ArgumentParser(
    description="Check macro-native migrated Morpho selector slice against spec and baseline."
  )
  p.add_argument("--spec", type=pathlib.Path, default=SPEC_PATH)
  p.add_argument("--macro", type=pathlib.Path, default=MACRO_PATH)
  p.add_argument("--baseline", type=pathlib.Path, default=BASELINE_PATH)
  p.add_argument("--contract", default=DEFAULT_CONTRACT)
  p.add_argument("--json-out", type=pathlib.Path)
  p.add_argument(
    "--write",
    action="store_true",
    help="Update baseline expectedMigrated set from current macro slice source",
  )
  return p


def main() -> None:
  args = parser().parse_args()

  macro_signatures = extract_macro_signatures(read_text(args.macro), contract_name=args.contract)
  if args.write:
    spec_signatures = extract_spec_signatures(read_text(args.spec))
    existing_baseline = load_baseline(args.baseline) if args.baseline.exists() else None
    baseline = build_write_baseline(
      spec_signatures=spec_signatures,
      migrated_signatures=macro_signatures,
      macro_path=args.macro,
      contract_name=args.contract,
      existing_baseline=existing_baseline,
    )
    write_baseline(args.baseline, baseline)

  report = run_check(
    spec_path=args.spec,
    macro_path=args.macro,
    baseline_path=args.baseline,
    contract_name=args.contract,
  )

  if args.json_out:
    try:
      args.json_out.parent.mkdir(parents=True, exist_ok=True)
      with args.json_out.open("w", encoding="utf-8") as f:
        json.dump(report, f, indent=2, sort_keys=True)
        f.write("\n")
    except OSError as exc:
      raise MigrationSliceError(f"failed to write JSON report {args.json_out}: {exc}") from exc

  print("macro-migration-slice check: OK")
  print(f"contract: {report['contract']}")
  print(
    "selector coverage: "
    f"{report['migratedCount']}/{report['specSignatureCount']} ({report['coveragePct']}%)"
  )
  print(f"migrated signatures: {', '.join(report['migratedSignatures'])}")


if __name__ == "__main__":
  try:
    main()
  except MigrationSliceError as e:
    print(f"macro-migration-slice check failed: {e}", file=sys.stderr)
    sys.exit(1)
