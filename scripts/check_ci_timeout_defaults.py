#!/usr/bin/env python3
"""Fail-closed sync check for CI timeout defaults vs workflow usage."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from workflow_run_parser import extract_workflow_env_literals, extract_workflow_run_text

RUN_WITH_TIMEOUT_RE = re.compile(r"run_with_timeout\.sh\s+([A-Z0-9_]+)\s+([0-9]+)\b")
SCRIPT_TIMEOUT_RE = re.compile(r"\b([A-Z0-9_]*TIMEOUT[A-Z0-9_]*)\b")
LINE_CONTINUATION_RE = re.compile(r"\\\s*\n\s*")
TIMEOUT_LITERAL_RE = re.compile(r"^[0-9]+$")

IGNORED_VARS = {
  # This timeout is intentionally overridden in timeout-wrapper tests.
  "MORPHO_TIMEOUT_KILL_AFTER_SEC",
}

# Inner wrapper timeout budgets must not exceed their enclosing job-step budgets.
NESTED_TIMEOUT_INVARIANTS = {
  "MORPHO_YUL_IDENTITY_TIMEOUT_SEC": {"MORPHO_VERITY_PREP_TIMEOUT_SEC"},
  "MORPHO_VERITY_PARITY_CHECK_TIMEOUT_SEC": {"MORPHO_VERITY_PREP_TIMEOUT_SEC"},
}


def fail(msg: str) -> None:
  print(f"ci-timeout-defaults check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def parse_timeout_env_file(path: pathlib.Path) -> dict[str, int]:
  values: dict[str, int] = {}
  for lineno, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
    line = raw.strip()
    if not line or line.startswith("#"):
      continue
    if "=" not in line:
      fail(f"{path}:{lineno}: expected KEY=VALUE format")
    key, value = line.split("=", 1)
    key = key.strip()
    value = value.strip()
    if not re.fullmatch(r"[A-Z_][A-Z0-9_]*", key):
      fail(f"{path}:{lineno}: invalid key {key!r}")
    if not re.fullmatch(r"[0-9]+", value):
      fail(f"{path}:{lineno}: value for {key} must be an integer (got {value!r})")
    values[key] = int(value)
  if not values:
    fail(f"{path}: no values parsed")
  return values


def collect_run_timeout_defaults(workflow_text: str) -> dict[str, set[int]]:
  normalized = LINE_CONTINUATION_RE.sub(" ", extract_workflow_run_text(workflow_text))
  seen: dict[str, set[int]] = {}
  for var, default in RUN_WITH_TIMEOUT_RE.findall(normalized):
    seen.setdefault(var, set()).add(int(default))
  return seen


def collect_timeout_env_literals(workflow_text: str) -> dict[str, set[int]]:
  seen: dict[str, set[int]] = {}
  for var, values in extract_workflow_env_literals(workflow_text).items():
    if "TIMEOUT" not in var:
      continue
    for value in values:
      if TIMEOUT_LITERAL_RE.fullmatch(value):
        seen.setdefault(var, set()).add(int(value))
  return seen


def collect_script_timeout_refs(scripts_dir: pathlib.Path) -> set[str]:
  seen: set[str] = set()
  for path in scripts_dir.glob("*"):
    if not path.is_file() or path.name.startswith("test_") or path.suffix not in {".py", ".sh"}:
      continue
    text = path.read_text(encoding="utf-8")
    seen.update(SCRIPT_TIMEOUT_RE.findall(text))
  return seen


def main() -> int:
  parser = argparse.ArgumentParser(description="Validate CI timeout defaults stay in sync with workflow")
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=pathlib.Path(".github/workflows/verify.yml"),
    help="Path to workflow yaml",
  )
  parser.add_argument(
    "--defaults",
    type=pathlib.Path,
    default=pathlib.Path("config/ci-timeout-defaults.env"),
    help="Path to timeout defaults env file",
  )
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=pathlib.Path("scripts"),
    help="Path to scripts directory",
  )
  args = parser.parse_args()

  workflow_text = args.workflow.read_text(encoding="utf-8")
  timeout_defaults = parse_timeout_env_file(args.defaults)
  run_defaults = collect_run_timeout_defaults(workflow_text)
  env_literals = collect_timeout_env_literals(workflow_text)
  script_refs = collect_script_timeout_refs(args.scripts_dir)

  missing_in_defaults = sorted(var for var in run_defaults if var not in timeout_defaults)
  if missing_in_defaults:
    fail(f"run_with_timeout vars missing from defaults: {', '.join(missing_in_defaults)}")

  mismatched_run_defaults: list[str] = []
  for var, literals in run_defaults.items():
    if var in IGNORED_VARS:
      continue
    if len(literals) != 1:
      rendered = ", ".join(str(v) for v in sorted(literals))
      mismatched_run_defaults.append(f"{var} has inconsistent run_with_timeout literals: {rendered}")
      continue
    literal = next(iter(literals))
    expected = timeout_defaults[var]
    if literal != expected:
      mismatched_run_defaults.append(
        f"{var} default mismatch: run_with_timeout uses {literal}, defaults file has {expected}"
      )
  if mismatched_run_defaults:
    fail("; ".join(mismatched_run_defaults))

  inconsistent_env_literals: list[str] = []
  for var, literals in sorted(env_literals.items()):
    if var in IGNORED_VARS:
      continue
    if var not in timeout_defaults:
      inconsistent_env_literals.append(f"{var} appears in workflow env but is missing from defaults file")
      continue
    expected = timeout_defaults[var]
    if expected not in literals:
      rendered = ", ".join(str(v) for v in sorted(literals))
      inconsistent_env_literals.append(
        f"{var} workflow env literals [{rendered}] do not include defaults value {expected}"
      )
  if inconsistent_env_literals:
    fail("; ".join(inconsistent_env_literals))

  nested_invariant_failures: list[str] = []
  for outer, inners in sorted(NESTED_TIMEOUT_INVARIANTS.items()):
    outer_value = timeout_defaults.get(outer)
    if outer_value is None:
      nested_invariant_failures.append(f"{outer} missing from defaults file")
      continue
    for inner in sorted(inners):
      inner_value = timeout_defaults.get(inner)
      if inner_value is None:
        nested_invariant_failures.append(f"{inner} missing from defaults file")
        continue
      if inner_value > outer_value:
        nested_invariant_failures.append(
          f"nested-timeout invariant violated: {inner}={inner_value} exceeds {outer}={outer_value}"
        )
  if nested_invariant_failures:
    fail("; ".join(nested_invariant_failures))

  used_timeout_vars = set(run_defaults) | set(env_literals) | script_refs
  stale_defaults = sorted(
    var for var in timeout_defaults if var not in used_timeout_vars and var not in IGNORED_VARS
  )
  if stale_defaults:
    fail("timeout defaults include stale/unused vars: " + ", ".join(stale_defaults))

  print(
    "ci-timeout-defaults: "
    "defaults="
    f"{len(timeout_defaults)} run_with_timeout_vars={len(run_defaults)} "
    f"env_timeout_vars={len(env_literals)} script_timeout_vars={len(script_refs)}"
  )
  print("ci-timeout-defaults check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
