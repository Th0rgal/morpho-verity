#!/usr/bin/env python3
"""Fail-closed check that workflow uses: references stay on the approved allowlist."""

from __future__ import annotations

import argparse
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
APPROVED_ACTION_REFS = frozenset(
  {
    "actions/cache@v4",
    "actions/checkout@v5",
    "actions/download-artifact@v4",
    "actions/upload-artifact@v4",
  }
)


class CiWorkflowActionAllowlistError(RuntimeError):
  """Raised when workflow action allowlist validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-action-allowlist check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowActionAllowlistError(
      f"workflow {path} is not valid UTF-8: {exc}"
    ) from exc
  except OSError as exc:
    raise CiWorkflowActionAllowlistError(f"failed to read workflow {path}: {exc}") from exc


def _strip_yaml_comment(value: str) -> str:
  return value.split("#", 1)[0].rstrip()


def _strip_yaml_scalar(value: str) -> str:
  value = _strip_yaml_comment(value).strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    return value[1:-1]
  return value


def collect_external_uses_references(workflow_text: str) -> list[str]:
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
      raise CiWorkflowActionAllowlistError("workflow uses: reference must be a non-empty literal")
    if uses_ref.startswith("./") or uses_ref.startswith("../") or uses_ref.startswith("docker://"):
      continue
    references.append(uses_ref)
  return references


def validate_action_allowlist(uses_references: list[str]) -> None:
  unexpected_refs = sorted(set(uses_references) - APPROVED_ACTION_REFS)
  if unexpected_refs:
    raise CiWorkflowActionAllowlistError(
      "workflow uses: references must stay inside the approved allowlist; "
      f"unexpected references: {', '.join(unexpected_refs)}"
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml uses: references stay on the approved action allowlist"
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
    uses_references = collect_external_uses_references(workflow_text)
    validate_action_allowlist(uses_references)
  except CiWorkflowActionAllowlistError as exc:
    fail(str(exc))

  print(
    "ci-workflow-action-allowlist: "
    f"approved_refs={len(set(uses_references))} total_uses_refs={len(uses_references)}"
  )
  print("ci-workflow-action-allowlist check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
