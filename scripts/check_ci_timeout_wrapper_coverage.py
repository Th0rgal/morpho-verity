#!/usr/bin/env python3
"""Fail-closed check that CI check scripts run under the timeout wrapper."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

WORKFLOW_CHECK_REF_RE = re.compile(r"\bscripts/(check_[A-Za-z0-9_]+\.(?:py|sh))\b")
RUN_WITH_TIMEOUT_STEP_RE = re.compile(
  r"run_with_timeout\.sh[^\n]*[ \t]+(?:--[ \t]+)?(?:python3[ \t]+)?"
  r"(?:\./)?scripts/(check_[A-Za-z0-9_]+\.(?:py|sh))\b"
)
LINE_CONTINUATION_RE = re.compile(r"\\\s*\n\s*")
RUN_STEP_RE = re.compile(r"^(\s*)run:\s*(.*)$")
RUN_BLOCK_SCALAR_RE = re.compile(r"^[|>][-+]?$")


def fail(msg: str) -> None:
  print(f"ci-timeout-wrapper-coverage check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def extract_workflow_run_text(workflow_text: str) -> str:
  commands: list[str] = []
  lines = workflow_text.splitlines()
  i = 0
  while i < len(lines):
    line = lines[i]
    match = RUN_STEP_RE.match(line)
    if match is None:
      i += 1
      continue

    run_indent = len(match.group(1))
    tail = match.group(2).strip()
    if tail and not RUN_BLOCK_SCALAR_RE.fullmatch(tail):
      line_parts = [tail]
      i += 1
      while i < len(lines):
        candidate = lines[i]
        stripped = candidate.lstrip(" ")
        if not stripped:
          break
        indent = len(candidate) - len(stripped)
        if indent <= run_indent:
          break
        line_parts.append(stripped)
        i += 1
      commands.append("\n".join(line_parts))
      continue

    i += 1
    block_lines: list[str] = []
    while i < len(lines):
      candidate = lines[i]
      stripped = candidate.lstrip(" ")
      if stripped:
        indent = len(candidate) - len(stripped)
        if indent <= run_indent:
          break
        block_lines.append(stripped)
      else:
        block_lines.append("")
      i += 1
    commands.append("\n".join(block_lines))
  return "\n".join(commands)


def collect_workflow_check_scripts(workflow_text: str) -> set[str]:
  run_text = extract_workflow_run_text(workflow_text)
  return {match.group(1) for match in WORKFLOW_CHECK_REF_RE.finditer(run_text)}


def collect_wrapped_check_scripts(workflow_text: str) -> set[str]:
  run_text = extract_workflow_run_text(workflow_text)
  normalized = LINE_CONTINUATION_RE.sub(" ", run_text)
  return {match.group(1) for match in RUN_WITH_TIMEOUT_STEP_RE.finditer(normalized)}


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate CI check scripts are protected by run_with_timeout.sh"
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
    help="check_*.{py,sh} script name allowed to be referenced without run_with_timeout.sh",
  )
  args = parser.parse_args()

  workflow_text = args.workflow.read_text(encoding="utf-8")
  workflow_checks = collect_workflow_check_scripts(workflow_text)
  wrapped_checks = collect_wrapped_check_scripts(workflow_text)
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
