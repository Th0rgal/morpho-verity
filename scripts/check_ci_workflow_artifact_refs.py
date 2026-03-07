#!/usr/bin/env python3
"""Fail-closed check that workflow artifact names stay internally consistent."""

from __future__ import annotations

import argparse
import pathlib
import sys

from check_ci_workflow_job_refs import collect_workflow_job_graph

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


def _collect_job_blocks(lines: list[str]) -> list[tuple[str, list[str]]]:
  jobs_index: int | None = None
  for index, line in enumerate(lines):
    if line.strip() == "jobs:" and len(line) - len(line.lstrip()) == 0:
      jobs_index = index
      break
  if jobs_index is None:
    raise CiWorkflowArtifactRefsError("workflow is missing top-level jobs: section")

  blocks: list[tuple[str, list[str]]] = []
  current_job_id: str | None = None
  current_block: list[str] = []

  for line in lines[jobs_index + 1:]:
    stripped = line.strip()
    indent = len(line) - len(line.lstrip())

    if indent == 0 and stripped and not stripped.startswith("#"):
      break
    if indent == 2 and stripped.endswith(":"):
      candidate = _strip_yaml_scalar(stripped[:-1]).strip()
      if not candidate:
        continue
      if current_job_id is not None:
        blocks.append((current_job_id, current_block))
      current_job_id = candidate
      current_block = []
      continue
    if current_job_id is not None:
      current_block.append(line)

  if current_job_id is not None:
    blocks.append((current_job_id, current_block))
  if not blocks:
    raise CiWorkflowArtifactRefsError("workflow jobs: section defines no jobs")
  return blocks


def collect_workflow_artifact_references(
  workflow_text: str,
) -> tuple[list[str], list[str], dict[str, list[str]], dict[str, list[str]]]:
  uploads: list[str] = []
  downloads: list[str] = []
  uploads_by_job: dict[str, list[str]] = {}
  downloads_by_job: dict[str, list[str]] = {}

  for job_id, block_lines in _collect_job_blocks(workflow_text.splitlines()):
    job_uploads: list[str] = []
    job_downloads: list[str] = []
    index = 0

    while index < len(block_lines):
      line = block_lines[index]
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

      while index < len(block_lines):
        line = block_lines[index]
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
        job_uploads.append(artifact_name)
      elif action == "download" and artifact_name is not None:
        if not artifact_name:
          raise CiWorkflowArtifactRefsError("download-artifact step has empty with.name")
        downloads.append(artifact_name)
        job_downloads.append(artifact_name)

    uploads_by_job[job_id] = job_uploads
    downloads_by_job[job_id] = job_downloads

  return uploads, downloads, uploads_by_job, downloads_by_job


def _collect_transitive_needs(job_id: str, needs_by_job: dict[str, list[str]]) -> set[str]:
  visited: set[str] = set()
  stack = list(needs_by_job.get(job_id, []))

  while stack:
    candidate = stack.pop()
    if candidate in visited:
      continue
    visited.add(candidate)
    stack.extend(needs_by_job.get(candidate, []))

  return visited


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
    upload_names, download_names, uploads_by_job, downloads_by_job = collect_workflow_artifact_references(
      workflow_text
    )
    _, needs_by_job = collect_workflow_job_graph(workflow_text)
  except CiWorkflowArtifactRefsError as exc:
    fail(str(exc))
  except RuntimeError as exc:
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

  producer_by_artifact = {
    artifact_name: job_id
    for job_id, artifact_names in uploads_by_job.items()
    for artifact_name in artifact_names
  }
  missing_dependencies: list[str] = []
  for job_id, artifact_names in downloads_by_job.items():
    transitive_needs = _collect_transitive_needs(job_id, needs_by_job)
    for artifact_name in artifact_names:
      producer_job = producer_by_artifact[artifact_name]
      if producer_job == job_id or producer_job in transitive_needs:
        continue
      missing_dependencies.append(f"{artifact_name} ({job_id} needs {producer_job})")
  if missing_dependencies:
    fail(
      "workflow downloads artifacts without depending on producer jobs: "
      + ", ".join(sorted(missing_dependencies))
    )

  print(
    "ci-workflow-artifact-refs: "
    f"uploads={len(upload_names)} downloads={len(download_names)} "
    f"unique_uploads={len(set(upload_names))} jobs_with_downloads={sum(bool(v) for v in downloads_by_job.values())}"
  )
  print("ci-workflow-artifact-refs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
