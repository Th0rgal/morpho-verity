#!/usr/bin/env python3
"""Fail-closed sync check for CI timeout defaults vs workflow usage."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

RUN_WITH_TIMEOUT_RE = re.compile(r"run_with_timeout\.sh\s+([A-Z0-9_]+)\s+([0-9]+)\b")
ENV_TIMEOUT_RE = re.compile(r"\b([A-Z0-9_]*TIMEOUT[A-Z0-9_]*)\s*:\s*\"([0-9]+)\"")

IGNORED_VARS = {
  # This timeout is intentionally overridden in timeout-wrapper tests.
  "MORPHO_TIMEOUT_KILL_AFTER_SEC",
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
  seen: dict[str, set[int]] = {}
  for var, default in RUN_WITH_TIMEOUT_RE.findall(workflow_text):
    seen.setdefault(var, set()).add(int(default))
  return seen


def collect_timeout_env_literals(workflow_text: str) -> dict[str, set[int]]:
  seen: dict[str, set[int]] = {}
  for var, value in ENV_TIMEOUT_RE.findall(workflow_text):
    seen.setdefault(var, set()).add(int(value))
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
  args = parser.parse_args()

  workflow_text = args.workflow.read_text(encoding="utf-8")
  timeout_defaults = parse_timeout_env_file(args.defaults)
  run_defaults = collect_run_timeout_defaults(workflow_text)
  env_literals = collect_timeout_env_literals(workflow_text)

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

  print(
    "ci-timeout-defaults: "
    f"defaults={len(timeout_defaults)} run_with_timeout_vars={len(run_defaults)} env_timeout_vars={len(env_literals)}"
  )
  print("ci-timeout-defaults check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
