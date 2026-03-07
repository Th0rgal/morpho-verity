#!/usr/bin/env python3
"""Fail-closed check that workflow job ids and needs references stay valid."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
JOB_ID_RE = re.compile(r"^[A-Za-z0-9_-]+$")


class CiWorkflowJobRefsError(RuntimeError):
  """Raised when workflow job graph validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-job-refs check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowJobRefsError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiWorkflowJobRefsError(f"failed to read workflow {path}: {exc}") from exc


def _strip_yaml_comment(value: str) -> str:
  return value.split("#", 1)[0].rstrip()


def _strip_yaml_scalar(value: str) -> str:
  value = _strip_yaml_comment(value).strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    return value[1:-1]
  return value


def _parse_inline_needs(raw_value: str) -> list[str]:
  raw_value = _strip_yaml_comment(raw_value).strip()
  if not raw_value:
    return []
  if raw_value.startswith("["):
    if not raw_value.endswith("]"):
      raise CiWorkflowJobRefsError(f"needs list must close with ']': {raw_value}")
    body = raw_value[1:-1].strip()
    if not body:
      return []
    refs = [_strip_yaml_scalar(part) for part in body.split(",")]
  else:
    refs = [_strip_yaml_scalar(raw_value)]

  invalid = [ref for ref in refs if not ref or not JOB_ID_RE.fullmatch(ref)]
  if invalid:
    raise CiWorkflowJobRefsError("needs references invalid job ids: " + ", ".join(sorted(set(invalid))))
  return refs


def _collect_job_blocks(lines: list[str]) -> list[tuple[str, list[str]]]:
  jobs_index: int | None = None
  for index, line in enumerate(lines):
    if line.strip() == "jobs:" and len(line) - len(line.lstrip()) == 0:
      jobs_index = index
      break
  if jobs_index is None:
    raise CiWorkflowJobRefsError("workflow is missing top-level jobs: section")

  blocks: list[tuple[str, list[str]]] = []
  current_job_id: str | None = None
  current_block: list[str] = []

  for line in lines[jobs_index + 1:]:
    stripped = line.strip()
    indent = len(line) - len(line.lstrip())

    if indent == 0 and stripped and not stripped.startswith("#"):
      break
    if indent == 2 and stripped.endswith(":"):
      candidate = _strip_yaml_comment(stripped[:-1]).strip()
      if not candidate:
        continue
      if not JOB_ID_RE.fullmatch(candidate):
        raise CiWorkflowJobRefsError(f"invalid workflow job id: {candidate}")
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
    raise CiWorkflowJobRefsError("workflow jobs: section defines no jobs")
  return blocks


def collect_workflow_job_graph(workflow_text: str) -> tuple[list[str], dict[str, list[str]]]:
  job_ids: list[str] = []
  needs_by_job: dict[str, list[str]] = {}

  for job_id, block_lines in _collect_job_blocks(workflow_text.splitlines()):
    job_ids.append(job_id)
    needs: list[str] = []
    index = 0
    while index < len(block_lines):
      line = block_lines[index]
      stripped = line.strip()
      indent = len(line) - len(line.lstrip())
      if indent == 4 and stripped.startswith("needs:"):
        raw_value = stripped.split(":", 1)[1]
        if raw_value.strip():
          needs.extend(_parse_inline_needs(raw_value))
          index += 1
          continue

        index += 1
        while index < len(block_lines):
          nested_line = block_lines[index]
          nested_stripped = nested_line.strip()
          nested_indent = len(nested_line) - len(nested_line.lstrip())
          if nested_indent <= 4 and nested_stripped:
            break
          if nested_stripped.startswith("- "):
            needs.extend(_parse_inline_needs(nested_stripped[2:]))
          elif nested_stripped and not nested_stripped.startswith("#"):
            raise CiWorkflowJobRefsError(f"needs block must contain list items: {nested_stripped}")
          index += 1
        continue
      index += 1
    needs_by_job[job_id] = needs

  return job_ids, needs_by_job


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml job ids are unique and needs references stay in sync"
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
    job_ids, needs_by_job = collect_workflow_job_graph(workflow_text)
  except CiWorkflowJobRefsError as exc:
    fail(str(exc))

  seen: set[str] = set()
  duplicate_jobs: list[str] = []
  for job_id in job_ids:
    if job_id in seen and job_id not in duplicate_jobs:
      duplicate_jobs.append(job_id)
    seen.add(job_id)
  if duplicate_jobs:
    fail("workflow defines duplicate job ids: " + ", ".join(sorted(duplicate_jobs)))

  missing_refs = sorted(
    {
      ref
      for refs in needs_by_job.values()
      for ref in refs
      if ref not in seen
    }
  )
  if missing_refs:
    fail("workflow needs references undefined jobs: " + ", ".join(missing_refs))

  self_refs = sorted(job_id for job_id, refs in needs_by_job.items() if job_id in refs)
  if self_refs:
    fail("workflow jobs cannot need themselves: " + ", ".join(self_refs))

  print(
    "ci-workflow-job-refs: "
    f"jobs={len(job_ids)} jobs_with_needs={sum(bool(refs) for refs in needs_by_job.values())} "
    f"total_needs_refs={sum(len(refs) for refs in needs_by_job.values())}"
  )
  print("ci-workflow-job-refs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
