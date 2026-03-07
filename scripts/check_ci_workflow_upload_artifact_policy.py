#!/usr/bin/env python3
"""Fail-closed check that upload-artifact steps error on missing files."""

from __future__ import annotations

import argparse
import pathlib
import sys

from ci_workflow_helpers import read_text, strip_yaml_scalar


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
REQUIRED_IF_NO_FILES_FOUND = "error"


class CiWorkflowUploadArtifactPolicyError(RuntimeError):
  """Raised when upload-artifact policy validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-upload-artifact-policy check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def collect_upload_artifact_policies(workflow_text: str) -> list[tuple[str | None, str | None]]:
  policies: list[tuple[str | None, str | None]] = []
  lines = workflow_text.splitlines()
  index = 0

  while index < len(lines):
    line = lines[index]
    stripped = line.strip()
    step_indent = len(line) - len(line.lstrip())
    if not stripped.startswith("- "):
      index += 1
      continue

    step_lines: list[tuple[int, str]] = [(step_indent, stripped[2:].strip())]
    index += 1
    while index < len(lines):
      candidate = lines[index]
      candidate_stripped = candidate.strip()
      candidate_indent = len(candidate) - len(candidate.lstrip())
      if candidate_indent <= step_indent and candidate_stripped.startswith("- "):
        break
      step_lines.append((candidate_indent, candidate_stripped))
      index += 1

    is_upload_step = False
    artifact_name: str | None = None
    with_indent: int | None = None
    if_no_files_found: str | None = None

    for indent, candidate in step_lines:
      if candidate.startswith("uses:"):
        action_ref = strip_yaml_scalar(candidate.split(":", 1)[1])
        is_upload_step = action_ref.startswith("actions/upload-artifact@")
      elif candidate == "with:":
        with_indent = indent
      elif with_indent is not None and indent <= with_indent:
        with_indent = None
      elif with_indent is not None and candidate.startswith("name:"):
        artifact_name = strip_yaml_scalar(candidate.split(":", 1)[1])
      elif with_indent is not None and candidate.startswith("if-no-files-found:"):
        if_no_files_found = strip_yaml_scalar(candidate.split(":", 1)[1])

    if is_upload_step:
      policies.append((artifact_name, if_no_files_found))

  return policies


def validate_upload_artifact_policy(artifact_name: str | None, if_no_files_found: str | None) -> None:
  display_name = artifact_name if artifact_name else "<unnamed>"
  if if_no_files_found is None:
    raise CiWorkflowUploadArtifactPolicyError(
      f"upload-artifact step {display_name} must set with.if-no-files-found: error"
    )
  if not if_no_files_found:
    raise CiWorkflowUploadArtifactPolicyError(
      f"upload-artifact step {display_name} has empty with.if-no-files-found"
    )
  if "${{" in if_no_files_found:
    raise CiWorkflowUploadArtifactPolicyError(
      "upload-artifact with.if-no-files-found must be a literal value, got expression: "
      f"{if_no_files_found}"
    )
  if if_no_files_found != REQUIRED_IF_NO_FILES_FOUND:
    raise CiWorkflowUploadArtifactPolicyError(
      f"upload-artifact step {display_name} must set with.if-no-files-found to "
      f"{REQUIRED_IF_NO_FILES_FOUND!r}, got {if_no_files_found!r}"
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml upload-artifact steps fail closed on missing files"
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
    workflow_text = read_text(workflow_path, CiWorkflowUploadArtifactPolicyError)
    policies = collect_upload_artifact_policies(workflow_text)
    for artifact_name, if_no_files_found in policies:
      validate_upload_artifact_policy(artifact_name, if_no_files_found)
  except CiWorkflowUploadArtifactPolicyError as exc:
    fail(str(exc))

  print(f"ci-workflow-upload-artifact-policy: upload_steps={len(policies)}")
  print("ci-workflow-upload-artifact-policy check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
