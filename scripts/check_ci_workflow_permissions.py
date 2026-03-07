#!/usr/bin/env python3
"""Fail-closed check that the workflow declares minimal GitHub token permissions."""

from __future__ import annotations

import argparse
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
EXPECTED_TOP_LEVEL_PERMISSIONS = {"contents": "read"}


class CiWorkflowPermissionsError(RuntimeError):
  """Raised when workflow permissions validation cannot complete safely."""


def fail(message: str) -> None:
  print(f"ci-workflow-permissions check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowPermissionsError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiWorkflowPermissionsError(f"failed to read workflow {path}: {exc}") from exc


def _indent_of(line: str) -> int:
  return len(line) - len(line.lstrip())


def _strip_yaml_comment(value: str) -> str:
  return value.split("#", 1)[0].rstrip()


def _strip_yaml_scalar(value: str) -> str:
  value = _strip_yaml_comment(value).strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    return value[1:-1]
  return value


def collect_top_level_permissions(workflow_text: str) -> dict[str, str]:
  lines = workflow_text.splitlines()
  permissions: dict[str, str] | None = None

  for index, line in enumerate(lines):
    stripped = line.strip()
    indent = _indent_of(line)
    if stripped == "jobs:" and indent == 0:
      break
    if indent != 0 or not stripped or stripped.startswith("#"):
      continue
    if not stripped.startswith("permissions:"):
      continue
    if permissions is not None:
      raise CiWorkflowPermissionsError("workflow defines multiple top-level permissions: entries")
    raw_value = stripped.split(":", 1)[1].strip()
    if raw_value:
      raise CiWorkflowPermissionsError(
        "workflow top-level permissions: must use a mapping, not a scalar value"
      )

    permissions = {}
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
        raise CiWorkflowPermissionsError(
          "workflow top-level permissions: must contain simple scope: access mappings"
        )
      scope, raw_access = candidate_stripped.split(":", 1)
      access = _strip_yaml_scalar(raw_access)
      if not access:
        raise CiWorkflowPermissionsError(
          f"workflow permission {scope.strip()} must have a non-empty literal access level"
        )
      permissions[_strip_yaml_scalar(scope)] = access
      cursor += 1

  if permissions is None:
    raise CiWorkflowPermissionsError("workflow is missing a top-level permissions: block")
  if not permissions:
    raise CiWorkflowPermissionsError("workflow top-level permissions: block must not be empty")
  return permissions


def collect_job_level_permissions(workflow_text: str) -> list[str]:
  lines = workflow_text.splitlines()
  in_jobs = False
  job_ids: list[str] = []
  current_job: str | None = None
  overridden_jobs: list[str] = []

  for line in lines:
    stripped = line.strip()
    indent = _indent_of(line)
    if not in_jobs:
      if stripped == "jobs:" and indent == 0:
        in_jobs = True
      continue
    if indent == 0 and stripped and not stripped.startswith("#"):
      break
    if indent == 2 and stripped.endswith(":") and not stripped.startswith("-"):
      current_job = _strip_yaml_scalar(stripped[:-1])
      job_ids.append(current_job)
      continue
    if current_job is not None and indent == 4 and stripped.startswith("permissions:"):
      overridden_jobs.append(current_job)

  return overridden_jobs


def validate_permissions(
  top_level_permissions: dict[str, str], job_level_permissions: list[str]
) -> list[str]:
  errors: list[str] = []
  if top_level_permissions != EXPECTED_TOP_LEVEL_PERMISSIONS:
    actual = ", ".join(
      f"{scope}={access}" for scope, access in sorted(top_level_permissions.items())
    )
    expected = ", ".join(
      f"{scope}={access}" for scope, access in sorted(EXPECTED_TOP_LEVEL_PERMISSIONS.items())
    )
    errors.append(
      f"workflow top-level permissions must be exactly {expected}, found: {actual}"
    )
  if job_level_permissions:
    errors.append(
      "workflow jobs must not override top-level permissions: "
      + ", ".join(sorted(job_level_permissions))
    )
  return errors


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml declares minimal top-level GitHub token permissions"
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
    top_level_permissions = collect_top_level_permissions(workflow_text)
    job_level_permissions = collect_job_level_permissions(workflow_text)
  except CiWorkflowPermissionsError as exc:
    fail(str(exc))

  errors = validate_permissions(top_level_permissions, job_level_permissions)
  if errors:
    fail("; ".join(errors))

  print(
    "ci-workflow-permissions: "
    f"top_level_scopes={len(top_level_permissions)} job_overrides={len(job_level_permissions)}"
  )
  print("ci-workflow-permissions check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
