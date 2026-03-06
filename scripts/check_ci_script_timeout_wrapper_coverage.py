#!/usr/bin/env python3
"""Fail-closed check that CI script invocations run under timeout wrapper."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

WORKFLOW_SCRIPT_REF_RE = re.compile(
  r"\b(?:python3\s+)?(?:\./)?scripts/([A-Za-z0-9_]+\.(?:py|sh))\b"
)
RUN_WITH_TIMEOUT_TARGET_RE = re.compile(
  r"(?:\./|\.\./)?scripts/run_with_timeout\.sh[^\n]*\s--\s+"
  r"(?:(?:python3\s+)(?:\./)?scripts/|(?:\./)?scripts/)"
  r"([A-Za-z0-9_]+\.(?:py|sh))\b"
)


def fail(msg: str) -> None:
  print(f"ci-script-timeout-wrapper-coverage check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def collect_workflow_script_references(workflow_text: str) -> set[str]:
  return {match.group(1) for match in WORKFLOW_SCRIPT_REF_RE.finditer(workflow_text)}


def collect_wrapped_script_targets(workflow_text: str) -> set[str]:
  return {match.group(1) for match in RUN_WITH_TIMEOUT_TARGET_RE.finditer(workflow_text)}


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate CI script invocations are protected by run_with_timeout.sh"
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=pathlib.Path(".github/workflows/verify.yml"),
    help="Path to workflow yaml",
  )
  parser.add_argument(
    "--allow-unwrapped",
    action="append",
    default=[],
    help="script name allowed to be referenced without run_with_timeout.sh",
  )
  args = parser.parse_args()

  workflow_text = args.workflow.read_text(encoding="utf-8")
  workflow_refs = collect_workflow_script_references(workflow_text)
  wrapped_targets = collect_wrapped_script_targets(workflow_text)
  allowed = set(args.allow_unwrapped)

  workflow_refs.discard("run_with_timeout.sh")
  unwrapped = sorted((workflow_refs - wrapped_targets) - allowed)
  if unwrapped:
    fail("workflow scripts not wrapped by run_with_timeout.sh: " + ", ".join(unwrapped))

  stale_allowlist = sorted(allowed - workflow_refs)
  if stale_allowlist:
    fail("allowlist entries not present in workflow: " + ", ".join(stale_allowlist))

  print(
    "ci-script-timeout-wrapper-coverage: "
    f"workflow_refs={len(workflow_refs)} wrapped_targets={len(wrapped_targets)}"
  )
  print("ci-script-timeout-wrapper-coverage check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
