#!/usr/bin/env python3
"""Fail-closed check that workflow uses: references stay well-formed."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
ACTION_METADATA_FILES = ("action.yml", "action.yaml", "Dockerfile")
EXTERNAL_ACTION_RE = re.compile(
  r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-/]+)?@[^\s@]+$"
)


class CiWorkflowUsesRefsError(RuntimeError):
  """Raised when workflow uses reference validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-uses-refs check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowUsesRefsError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiWorkflowUsesRefsError(f"failed to read workflow {path}: {exc}") from exc


def _strip_yaml_comment(value: str) -> str:
  return value.split("#", 1)[0].rstrip()


def _strip_yaml_scalar(value: str) -> str:
  value = _strip_yaml_comment(value).strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    return value[1:-1]
  return value


def collect_workflow_uses_references(workflow_text: str) -> list[str]:
  references: list[str] = []
  for raw_line in workflow_text.splitlines():
    stripped = raw_line.lstrip()
    if not stripped or stripped.startswith("#"):
      continue
    if stripped.startswith("- "):
      stripped = stripped[2:].lstrip()
    if not stripped.startswith("uses:"):
      continue
    uses_ref = _strip_yaml_scalar(stripped.split(":", 1)[1])
    if not uses_ref:
      raise CiWorkflowUsesRefsError("workflow uses: reference must be a non-empty literal")
    references.append(uses_ref)
  return references


def resolve_local_action_path(repo_root: pathlib.Path, uses_ref: str) -> pathlib.Path:
  relative_path = pathlib.PurePosixPath(uses_ref)
  resolved = (repo_root / relative_path).resolve()
  try:
    resolved.relative_to(repo_root.resolve())
  except ValueError as exc:
    raise CiWorkflowUsesRefsError(
      f"workflow local action references must stay inside the repository: {uses_ref}"
    ) from exc
  return resolved


def validate_uses_reference(repo_root: pathlib.Path, uses_ref: str) -> None:
  if "${{" in uses_ref:
    raise CiWorkflowUsesRefsError(
      f"workflow uses: references must be literal values, got expression: {uses_ref}"
    )

  if uses_ref.startswith("docker://"):
    if uses_ref == "docker://":
      raise CiWorkflowUsesRefsError("workflow docker uses: reference must include an image")
    return

  if uses_ref.startswith("./") or uses_ref.startswith("../"):
    resolved = resolve_local_action_path(repo_root, uses_ref)
    if not resolved.is_dir():
      raise CiWorkflowUsesRefsError(
        f"workflow references missing local action path: {uses_ref}"
      )
    if not any((resolved / name).exists() for name in ACTION_METADATA_FILES):
      raise CiWorkflowUsesRefsError(
        "workflow local action is missing action metadata: "
        f"{uses_ref} (expected one of {', '.join(ACTION_METADATA_FILES)})"
      )
    return

  if not EXTERNAL_ACTION_RE.match(uses_ref):
    raise CiWorkflowUsesRefsError(
      "workflow external uses: references must match owner/repo[/path]@ref: "
      f"{uses_ref}"
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml uses: references are literal and well-formed"
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=WORKFLOW_PATH,
    help="Path to workflow yaml",
  )
  parser.add_argument(
    "--repo-root",
    type=pathlib.Path,
    default=ROOT,
    help="Path to repository root used to resolve local action paths",
  )
  args = parser.parse_args()
  workflow_path = args.workflow.resolve()
  repo_root = args.repo_root.resolve()

  if not repo_root.is_dir():
    fail(f"repository root does not exist: {repo_root}")

  try:
    workflow_text = read_text(workflow_path)
    uses_references = collect_workflow_uses_references(workflow_text)
    for uses_ref in uses_references:
      validate_uses_reference(repo_root, uses_ref)
  except CiWorkflowUsesRefsError as exc:
    fail(str(exc))

  print(f"ci-workflow-uses-refs: uses_refs={len(uses_references)}")
  print("ci-workflow-uses-refs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
