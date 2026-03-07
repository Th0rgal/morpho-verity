#!/usr/bin/env python3
"""Fail-closed check that the workflow declares explicit top-level concurrency."""

from __future__ import annotations

import argparse
import pathlib
import sys

from ci_workflow_helpers import read_text, strip_yaml_scalar


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
EXPECTED_CONCURRENCY = {
  "group": "${{ github.workflow }}-${{ github.ref }}-${{ github.event_name }}",
  "cancel-in-progress": "true",
}


class CiWorkflowConcurrencyError(RuntimeError):
  """Raised when workflow concurrency validation cannot complete safely."""


def fail(message: str) -> None:
  print(f"ci-workflow-concurrency check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def _indent_of(line: str) -> int:
  return len(line) - len(line.lstrip())


def collect_top_level_concurrency(workflow_text: str) -> dict[str, str]:
  lines = workflow_text.splitlines()
  concurrency: dict[str, str] | None = None

  for index, line in enumerate(lines):
    stripped = line.strip()
    indent = _indent_of(line)
    if stripped == "jobs:" and indent == 0:
      break
    if indent != 0 or not stripped or stripped.startswith("#"):
      continue
    if not stripped.startswith("concurrency:"):
      continue
    if concurrency is not None:
      raise CiWorkflowConcurrencyError("workflow defines multiple top-level concurrency: entries")
    raw_value = stripped.split(":", 1)[1].strip()
    if raw_value:
      raise CiWorkflowConcurrencyError(
        "workflow top-level concurrency: must use a mapping, not a scalar value"
      )

    concurrency = {}
    cursor = index + 1
    while cursor < len(lines):
      candidate = lines[cursor]
      candidate_stripped = candidate.strip()
      candidate_indent = _indent_of(candidate)
      if candidate_indent == 0 and candidate_stripped and not candidate_stripped.startswith("#"):
        break
      if not candidate_stripped or candidate_stripped.startswith("#"):
        cursor += 1
        continue
      if candidate_indent <= indent:
        break
      if candidate_indent != indent + 2 or ":" not in candidate_stripped:
        raise CiWorkflowConcurrencyError(
          "workflow top-level concurrency: must contain simple key: value mappings"
        )
      field, raw_setting = candidate_stripped.split(":", 1)
      setting = strip_yaml_scalar(raw_setting)
      if not setting:
        raise CiWorkflowConcurrencyError(
          f"workflow concurrency field {field.strip()} must have a non-empty literal value"
        )
      concurrency[strip_yaml_scalar(field)] = setting
      cursor += 1

  if concurrency is None:
    raise CiWorkflowConcurrencyError("workflow is missing a top-level concurrency: block")
  if not concurrency:
    raise CiWorkflowConcurrencyError("workflow top-level concurrency: block must not be empty")
  return concurrency


def validate_concurrency(concurrency: dict[str, str]) -> list[str]:
  errors: list[str] = []
  if concurrency != EXPECTED_CONCURRENCY:
    actual = ", ".join(f"{field}={value}" for field, value in sorted(concurrency.items()))
    expected = ", ".join(
      f"{field}={value}" for field, value in sorted(EXPECTED_CONCURRENCY.items())
    )
    errors.append(
      f"workflow top-level concurrency must be exactly {expected}, found: {actual}"
    )
  return errors


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml declares explicit top-level concurrency policy"
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
    workflow_text = read_text(workflow_path, CiWorkflowConcurrencyError)
    concurrency = collect_top_level_concurrency(workflow_text)
  except CiWorkflowConcurrencyError as exc:
    fail(str(exc))

  errors = validate_concurrency(concurrency)
  if errors:
    fail("; ".join(errors))

  print(f"ci-workflow-concurrency: fields={len(concurrency)}")
  print("ci-workflow-concurrency check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
