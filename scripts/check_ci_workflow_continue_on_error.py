#!/usr/bin/env python3
"""Fail-closed check that only allowlisted jobs use continue-on-error."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from check_ci_timeout_job_budget_fit import collect_job_blocks, read_text
from ci_workflow_helpers import strip_yaml_scalar

ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
JOB_CONTINUE_ON_ERROR_RE = re.compile(r"^(\s*)continue-on-error:\s*(.*?)\s*$")
ALLOWED_JOB_CONTINUE_ON_ERROR = frozenset(
  {"yul-identity-report", "verity-compiled-tests", "morpho-blue-parity"}
)


class CiWorkflowContinueOnErrorError(RuntimeError):
  """Raised when continue-on-error validation cannot complete safely."""


def fail(message: str) -> None:
  print(f"ci-workflow-continue-on-error check failed: {message}", file=sys.stderr)
  raise SystemExit(1)

def collect_job_continue_on_error_values(job_text: str) -> list[str]:
  values: list[str] = []
  for line in job_text.splitlines():
    match = JOB_CONTINUE_ON_ERROR_RE.match(line)
    if match is None:
      continue
    if len(match.group(1)) != 4:
      continue
    values.append(strip_yaml_scalar(match.group(2)))
  return values


def collect_step_level_continue_on_error_jobs(job_blocks: dict[str, str]) -> list[str]:
  offenders: list[str] = []
  for job_name, job_text in sorted(job_blocks.items()):
    for line in job_text.splitlines():
      match = JOB_CONTINUE_ON_ERROR_RE.match(line)
      if match is None:
        continue
      if len(match.group(1)) == 8:
        offenders.append(job_name)
        break
  return offenders


def validate_continue_on_error(job_blocks: dict[str, str]) -> list[str]:
  failures: list[str] = []
  for job_name in sorted(job_blocks):
    values = collect_job_continue_on_error_values(job_blocks[job_name])
    if not values:
      continue
    if len(values) != 1:
      rendered = ", ".join(values)
      failures.append(f"job {job_name} has multiple continue-on-error values: {rendered}")
      continue
    value = values[0]
    if not value:
      failures.append(f"job {job_name} has an empty continue-on-error value")
      continue
    if "${{" in value:
      failures.append(
        f"job {job_name} continue-on-error must be a literal boolean, found expression: {value}"
      )
      continue
    if value != "true":
      failures.append(
        f"job {job_name} continue-on-error must be literal true when present, found: {value}"
      )
      continue
    if job_name not in ALLOWED_JOB_CONTINUE_ON_ERROR:
      allowlist = ", ".join(sorted(ALLOWED_JOB_CONTINUE_ON_ERROR))
      failures.append(
        f"job {job_name} must not set continue-on-error; only allowlisted jobs may do so: "
        f"{allowlist}"
      )

  step_level = collect_step_level_continue_on_error_jobs(job_blocks)
  if step_level:
    failures.append(
      "workflow steps must not set continue-on-error: " + ", ".join(sorted(step_level))
    )
  return failures


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate only allowlisted verify.yml jobs use literal continue-on-error: true"
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
      raise CiWorkflowContinueOnErrorError("no CI jobs found in workflow")
    failures = validate_continue_on_error(job_blocks)
  except (CiWorkflowContinueOnErrorError, RuntimeError) as exc:
    fail(str(exc))

  if failures:
    fail("; ".join(failures))

  enabled_jobs = [
    job_name
    for job_name in sorted(job_blocks)
    if collect_job_continue_on_error_values(job_blocks[job_name])
  ]
  print(
    "ci-workflow-continue-on-error: "
    f"jobs={len(job_blocks)} continue_on_error_jobs={len(enabled_jobs)}"
  )
  print("ci-workflow-continue-on-error check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
