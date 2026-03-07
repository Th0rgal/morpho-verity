#!/usr/bin/env python3
"""Fail-closed check that workflow artifact names stay internally consistent."""

from __future__ import annotations

import argparse
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"


class CiWorkflowArtifactRefsError(RuntimeError):
  """Raised when workflow artifact reference validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-artifact-refs check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowArtifactRefsError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiWorkflowArtifactRefsError(f"failed to read workflow {path}: {exc}") from exc


def _strip_yaml_scalar(value: str) -> str:
  value = value.strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    return value[1:-1]
  return value


def collect_workflow_artifact_references(workflow_text: str) -> tuple[list[str], list[str]]:
  uploads: list[str] = []
  downloads: list[str] = []
  lines = workflow_text.splitlines()
  index = 0

  while index < len(lines):
    line = lines[index]
    stripped = line.strip()
    step_indent = len(line) - len(line.lstrip())
    if not stripped.startswith("- "):
      index += 1
      continue

    action: str | None = None
    artifact_name: str | None = None
    with_indent: int | None = None
    step_lines = [stripped[2:].strip()]
    index += 1

    while index < len(lines):
      line = lines[index]
      stripped = line.strip()
      indent = len(line) - len(line.lstrip())

      if indent <= step_indent and stripped.startswith("- "):
        break
      step_lines.append(stripped)
      index += 1

    for stripped in step_lines:
      if stripped.startswith("uses:"):
        action_ref = stripped.split(":", 1)[1].strip()
        if action_ref.startswith("actions/upload-artifact@"):
          action = "upload"
        elif action_ref.startswith("actions/download-artifact@"):
          action = "download"
      elif stripped == "with:":
        with_indent = 0
      elif with_indent is not None and stripped.startswith("name:"):
        artifact_name = _strip_yaml_scalar(stripped.split(":", 1)[1])
      elif with_indent is not None and stripped:
        with_indent = None

    if action == "upload":
      if artifact_name is None or not artifact_name:
        raise CiWorkflowArtifactRefsError("upload-artifact step missing non-empty with.name")
      uploads.append(artifact_name)
    elif action == "download" and artifact_name is not None:
      if not artifact_name:
        raise CiWorkflowArtifactRefsError("download-artifact step has empty with.name")
      downloads.append(artifact_name)

  return uploads, downloads


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml artifact upload/download names stay in sync"
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
    upload_names, download_names = collect_workflow_artifact_references(workflow_text)
  except CiWorkflowArtifactRefsError as exc:
    fail(str(exc))

  seen_uploads: set[str] = set()
  duplicate_uploads: list[str] = []
  for name in upload_names:
    if name in seen_uploads and name not in duplicate_uploads:
      duplicate_uploads.append(name)
    seen_uploads.add(name)
  if duplicate_uploads:
    fail("workflow uploads duplicate artifact names: " + ", ".join(sorted(duplicate_uploads)))

  missing_uploads = sorted(set(download_names) - set(upload_names))
  if missing_uploads:
    fail("workflow downloads artifacts with no matching upload step: " + ", ".join(missing_uploads))

  print(
    "ci-workflow-artifact-refs: "
    f"uploads={len(upload_names)} downloads={len(download_names)} unique_uploads={len(set(upload_names))}"
  )
  print("ci-workflow-artifact-refs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
