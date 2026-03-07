#!/usr/bin/env python3
"""Fail-closed check that CI check scripts run under the timeout wrapper."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from workflow_run_parser import extract_workflow_run_text

ROOT = pathlib.Path(__file__).resolve().parent.parent

WORKFLOW_CHECK_REF_RE = re.compile(
  r"\b(?:(?:python3|python|bash|sh)[ \t]+|env[ \t]+(?:python3|python|bash|sh)[ \t]+)?"
  r"[\"']?(?:\./)?scripts/(check_[A-Za-z0-9_]+\.(?:py|sh))\b[\"']?"
)
RUN_WITH_TIMEOUT_STEP_RE = re.compile(
  r"run_with_timeout\.sh[^\n]*[ \t]+(?:--[ \t]+)?"
  r"(?:(?:python3|python|bash|sh)[ \t]+|env[ \t]+(?:python3|python|bash|sh)[ \t]+)?"
  r"[\"']?(?:\./)?scripts/(check_[A-Za-z0-9_]+\.(?:py|sh))\b[\"']?"
)
LINE_CONTINUATION_RE = re.compile(r"\\\s*\n\s*")


class CiTimeoutWrapperCoverageError(RuntimeError):
  """Raised when timeout-wrapper coverage validation cannot complete safely."""


def fail(msg: str) -> None:
  print(f"ci-timeout-wrapper-coverage check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def read_workflow_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiTimeoutWrapperCoverageError(
      f"workflow file {path} is not valid UTF-8"
    ) from exc
  except OSError as exc:
    raise CiTimeoutWrapperCoverageError(
      f"failed to read workflow file {path}: {exc}"
    ) from exc


def collect_workflow_check_scripts(workflow_text: str) -> set[str]:
  try:
    run_text = extract_workflow_run_text(workflow_text)
  except ValueError as exc:
    raise CiTimeoutWrapperCoverageError(
      f"failed to parse workflow run steps: {exc}"
    ) from exc
  return {match.group(1) for match in WORKFLOW_CHECK_REF_RE.finditer(run_text)}


def collect_wrapped_check_scripts(workflow_text: str) -> set[str]:
  try:
    run_text = extract_workflow_run_text(workflow_text)
  except ValueError as exc:
    raise CiTimeoutWrapperCoverageError(
      f"failed to parse workflow run steps: {exc}"
    ) from exc
  normalized = LINE_CONTINUATION_RE.sub(" ", run_text)
  return {match.group(1) for match in RUN_WITH_TIMEOUT_STEP_RE.finditer(normalized)}


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate CI check scripts are protected by run_with_timeout.sh"
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=ROOT / ".github/workflows/verify.yml",
    help="Path to workflow yaml",
  )
  parser.add_argument(
    "--allow-unwrapped",
    action="append",
    default=[],
    help="check_*.{py,sh} script name allowed to be referenced without run_with_timeout.sh",
  )
  args = parser.parse_args()

  workflow_path = args.workflow.resolve()

  try:
    workflow_text = read_workflow_text(workflow_path)
    workflow_checks = collect_workflow_check_scripts(workflow_text)
    wrapped_checks = collect_wrapped_check_scripts(workflow_text)
  except CiTimeoutWrapperCoverageError as exc:
    fail(str(exc))
  allowed = set(args.allow_unwrapped)

  unwrapped = sorted((workflow_checks - wrapped_checks) - allowed)
  if unwrapped:
    fail("workflow check scripts not wrapped by run_with_timeout.sh: " + ", ".join(unwrapped))

  stale_allowlist = sorted(allowed - workflow_checks)
  if stale_allowlist:
    fail("allowlist entries not present in workflow: " + ", ".join(stale_allowlist))

  print(
    "ci-timeout-wrapper-coverage: "
    f"workflow_checks={len(workflow_checks)} wrapped_checks={len(wrapped_checks)}"
  )
  print("ci-timeout-wrapper-coverage check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
