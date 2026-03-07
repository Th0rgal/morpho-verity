#!/usr/bin/env python3
"""Fail-closed check that run_with_timeout budgets fit inside CI job timeouts."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from workflow_run_parser import extract_workflow_run_text

ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"

JOBS_FIELD_RE = re.compile(r"^(\s*)jobs:\s*$")
JOB_FIELD_RE = re.compile(r"^(\s*)([A-Za-z0-9_-]+):\s*$")
TIMEOUT_MINUTES_RE = re.compile(r"^(\s*)timeout-minutes:\s*([0-9]+)\s*$")
LINE_CONTINUATION_RE = re.compile(r"\\\s*\n\s*")
RUN_WITH_TIMEOUT_RE = re.compile(r"run_with_timeout\.sh\s+([A-Z0-9_]+)\s+([0-9]+)\b")


class CiTimeoutJobBudgetFitError(RuntimeError):
  """Raised when the checker cannot safely validate workflow timeout budgets."""


def fail(msg: str) -> None:
  print(f"ci-timeout-job-budget-fit check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiTimeoutJobBudgetFitError(f"workflow file {path} is not valid UTF-8") from exc
  except OSError as exc:
    raise CiTimeoutJobBudgetFitError(f"failed to read workflow file {path}: {exc}") from exc


def collect_job_blocks(workflow_text: str) -> dict[str, str]:
  lines = workflow_text.splitlines()
  jobs_indent: int | None = None
  current_job_name: str | None = None
  current_job_indent: int | None = None
  current_job_lines: list[str] = []
  blocks: dict[str, str] = {}

  for line in lines:
    stripped = line.lstrip(" ")
    indent = len(line) - len(stripped)

    jobs_match = JOBS_FIELD_RE.match(line)
    if jobs_match is not None:
      jobs_indent = len(jobs_match.group(1))
      current_job_name = None
      current_job_indent = None
      current_job_lines = []
      continue

    if jobs_indent is None:
      continue

    if stripped and indent <= jobs_indent:
      if current_job_name is not None:
        blocks[current_job_name] = "\n".join(current_job_lines)
      jobs_indent = None
      current_job_name = None
      current_job_indent = None
      current_job_lines = []
      continue

    job_match = JOB_FIELD_RE.match(line)
    if job_match is not None and len(job_match.group(1)) == jobs_indent + 2:
      if current_job_name is not None:
        blocks[current_job_name] = "\n".join(current_job_lines)
      current_job_name = job_match.group(2)
      current_job_indent = len(job_match.group(1))
      current_job_lines = [line]
      continue

    if current_job_name is not None and current_job_indent is not None:
      current_job_lines.append(line)

  if current_job_name is not None:
    blocks[current_job_name] = "\n".join(current_job_lines)
  return blocks


def parse_job_timeout_minutes(job_name: str, job_text: str) -> int:
  timeout_values: list[int] = []
  for line in job_text.splitlines():
    match = TIMEOUT_MINUTES_RE.match(line)
    if match is None:
      continue
    if len(match.group(1)) != 4:
      continue
    timeout_values.append(int(match.group(2)))
  if not timeout_values:
    raise CiTimeoutJobBudgetFitError(f"job {job_name} is missing timeout-minutes")
  if len(timeout_values) != 1:
    rendered = ", ".join(str(value) for value in timeout_values)
    raise CiTimeoutJobBudgetFitError(f"job {job_name} has multiple timeout-minutes values: {rendered}")
  return timeout_values[0]


def collect_job_run_with_timeout_literals(job_text: str) -> list[tuple[str, int]]:
  try:
    run_text = extract_workflow_run_text(job_text)
  except ValueError as exc:
    raise CiTimeoutJobBudgetFitError(f"failed to parse workflow run steps: {exc}") from exc
  normalized = LINE_CONTINUATION_RE.sub(" ", run_text)
  return [(var, int(literal)) for var, literal in RUN_WITH_TIMEOUT_RE.findall(normalized)]


def validate_job_timeout_budgets(job_blocks: dict[str, str]) -> list[str]:
  failures: list[str] = []
  for job_name in sorted(job_blocks):
    job_text = job_blocks[job_name]
    timeout_minutes = parse_job_timeout_minutes(job_name, job_text)
    timeout_seconds = timeout_minutes * 60
    for timeout_var, timeout_literal in collect_job_run_with_timeout_literals(job_text):
      if timeout_literal > timeout_seconds:
        failures.append(
          f"job {job_name} timeout-minutes={timeout_minutes} ({timeout_seconds}s) "
          f"is lower than {timeout_var}={timeout_literal}s"
        )
  return failures


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate run_with_timeout budgets fit inside enclosing CI job timeouts"
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
      raise CiTimeoutJobBudgetFitError("no CI jobs found in workflow")
    failures = validate_job_timeout_budgets(job_blocks)
  except CiTimeoutJobBudgetFitError as exc:
    fail(str(exc))

  if failures:
    fail("; ".join(failures))

  wrapped_steps = sum(len(collect_job_run_with_timeout_literals(text)) for text in job_blocks.values())
  print(
    "ci-timeout-job-budget-fit: "
    f"jobs={len(job_blocks)} wrapped_steps={wrapped_steps}"
  )
  print("ci-timeout-job-budget-fit check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
