#!/usr/bin/env python3
"""Fail-closed check that every CI job declares one literal timeout budget."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from check_ci_timeout_job_budget_fit import collect_job_blocks, read_text
from ci_workflow_helpers import strip_yaml_comment

ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"

TIMEOUT_FIELD_RE = re.compile(r"^(\s*)timeout-minutes:\s*(.*?)\s*$")


class CiWorkflowJobTimeoutsError(RuntimeError):
  """Raised when workflow job timeout validation cannot complete safely."""


def fail(message: str) -> None:
  print(f"ci-workflow-job-timeouts check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def collect_job_timeout_values(job_text: str) -> list[str]:
  values: list[str] = []
  for line in job_text.splitlines():
    match = TIMEOUT_FIELD_RE.match(line)
    if match is None:
      continue
    if len(match.group(1)) != 4:
      continue
    values.append(strip_yaml_comment(match.group(2)).strip())
  return values


def validate_job_timeout_value(job_name: str, raw_value: str) -> str | None:
  if not raw_value:
    return f"job {job_name} has an empty timeout-minutes value"
  if "${{" in raw_value:
    return f"job {job_name} timeout-minutes must be a literal integer, found expression: {raw_value}"
  if not re.fullmatch(r"[0-9]+", raw_value):
    return f"job {job_name} timeout-minutes must be an unquoted integer, found: {raw_value}"
  if int(raw_value) <= 0:
    return f"job {job_name} timeout-minutes must be greater than zero, found: {raw_value}"
  return None


def validate_job_timeouts(job_blocks: dict[str, str]) -> list[str]:
  failures: list[str] = []
  for job_name in sorted(job_blocks):
    timeout_values = collect_job_timeout_values(job_blocks[job_name])
    if not timeout_values:
      failures.append(f"job {job_name} is missing timeout-minutes")
      continue
    if len(timeout_values) != 1:
      rendered = ", ".join(timeout_values)
      failures.append(f"job {job_name} has multiple timeout-minutes values: {rendered}")
      continue
    failure = validate_job_timeout_value(job_name, timeout_values[0])
    if failure is not None:
      failures.append(failure)
  return failures


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate every verify.yml job declares one literal positive timeout-minutes value"
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=WORKFLOW_PATH,
    help="Path to workflow yaml",
  )
  args = parser.parse_args()
  workflow_path = args.workflow.resolve()

  try:
    workflow_text = read_text(workflow_path)
    job_blocks = collect_job_blocks(workflow_text)
    if not job_blocks:
      raise CiWorkflowJobTimeoutsError("no CI jobs found in workflow")
    failures = validate_job_timeouts(job_blocks)
  except (CiWorkflowJobTimeoutsError, RuntimeError) as exc:
    fail(str(exc))

  if failures:
    fail("; ".join(failures))

  print(f"ci-workflow-job-timeouts: jobs={len(job_blocks)}")
  print("ci-workflow-job-timeouts check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
