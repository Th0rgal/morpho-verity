#!/usr/bin/env python3
"""Fail-closed check that CI script invocations run under timeout wrapper."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from workflow_run_parser import extract_workflow_run_text

WORKFLOW_SCRIPT_REF_RE = re.compile(
  r"\b(?:(?:python3|python|bash|sh)\s+|env\s+(?:python3|python|bash|sh)\s+)?"
  r"[\"']?(?:\./)?scripts/([A-Za-z0-9_]+\.(?:py|sh))\b[\"']?"
)
RUN_WITH_TIMEOUT_TARGET_RE = re.compile(
  r"(?:\./|\.\./)?scripts/run_with_timeout\.sh[^\n]*[ \t]+--[ \t]+"
  r"(?:(?:(?:python3|python|bash|sh)[ \t]+|env[ \t]+(?:python3|python|bash|sh)[ \t]+)?[\"']?(?:\./)?scripts/)"
  r"([A-Za-z0-9_]+\.(?:py|sh))\b"
  r"[\"']?"
)
LINE_CONTINUATION_RE = re.compile(r"\\\s*\n\s*")
SHELL_TEST_LOOP_RE = re.compile(
  r"for\s+([A-Za-z_][A-Za-z0-9_]*)\s+in\s+scripts/test_\*\.sh\s*;\s*do(?P<body>.*?)\bdone\b",
  re.DOTALL,
)


class CiScriptTimeoutWrapperCoverageError(RuntimeError):
  pass


def fail(msg: str) -> None:
  print(f"ci-script-timeout-wrapper-coverage check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def read_workflow_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiScriptTimeoutWrapperCoverageError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiScriptTimeoutWrapperCoverageError(f"failed to read workflow {path}: {exc}") from exc


def collect_workflow_script_references(workflow_text: str) -> set[str]:
  try:
    run_text = extract_workflow_run_text(workflow_text)
  except ValueError as exc:
    raise CiScriptTimeoutWrapperCoverageError(f"failed to parse workflow run steps: {exc}") from exc
  return {match.group(1) for match in WORKFLOW_SCRIPT_REF_RE.finditer(run_text)}


def collect_wrapped_script_targets(workflow_text: str) -> set[str]:
  try:
    run_text = extract_workflow_run_text(workflow_text)
  except ValueError as exc:
    raise CiScriptTimeoutWrapperCoverageError(f"failed to parse workflow run steps: {exc}") from exc
  normalized = LINE_CONTINUATION_RE.sub(" ", run_text)
  return {match.group(1) for match in RUN_WITH_TIMEOUT_TARGET_RE.finditer(normalized)}


def collect_shell_test_loop_blocks(workflow_text: str) -> list[tuple[str, str]]:
  try:
    run_text = extract_workflow_run_text(workflow_text)
  except ValueError as exc:
    raise CiScriptTimeoutWrapperCoverageError(f"failed to parse workflow run steps: {exc}") from exc
  blocks: list[tuple[str, str]] = []
  for match in SHELL_TEST_LOOP_RE.finditer(run_text):
    loop_var = match.group(1)
    loop_body = match.group("body")
    blocks.append((loop_var, loop_body))
  return blocks


def collect_repo_shell_script_tests(scripts_dir: pathlib.Path) -> set[str]:
  tests: set[str] = set()
  for path in scripts_dir.glob("test_*.sh"):
    if path.is_file():
      tests.add(path.name)
  return tests


def loop_uses_timeout_wrapper(loop_var: str, loop_body: str) -> bool:
  var_expr_re = r"(?:\$\{" + re.escape(loop_var) + r"\}|\$" + re.escape(loop_var) + r")"
  wrapped_loop_target_re = re.compile(
    r"run_with_timeout\.sh[\s\S]*?--\s+[\"']?\./" + var_expr_re + r"[\"']?"
  )
  return bool(wrapped_loop_target_re.search(loop_body))


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
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=pathlib.Path("scripts"),
    help="Path to scripts directory",
  )
  args = parser.parse_args()

  try:
    workflow_text = read_workflow_text(args.workflow)
    workflow_refs = collect_workflow_script_references(workflow_text)
    wrapped_targets = collect_wrapped_script_targets(workflow_text)
    shell_test_loops = collect_shell_test_loop_blocks(workflow_text)
    shell_script_tests = collect_repo_shell_script_tests(args.scripts_dir)
  except CiScriptTimeoutWrapperCoverageError as exc:
    fail(str(exc))
  allowed = set(args.allow_unwrapped)

  workflow_refs.discard("run_with_timeout.sh")
  if shell_test_loops:
    workflow_refs.update(shell_script_tests)
    for loop_var, loop_body in shell_test_loops:
      if not loop_uses_timeout_wrapper(loop_var, loop_body):
        fail(f"shell test loop variable '{loop_var}' executes without run_with_timeout.sh")
    wrapped_targets.update(shell_script_tests)

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
