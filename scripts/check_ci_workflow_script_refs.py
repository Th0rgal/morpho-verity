#!/usr/bin/env python3
"""Fail-closed check that workflow script references exist in scripts/."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from workflow_run_parser import extract_workflow_run_text


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
SCRIPTS_DIR = ROOT / "scripts"
WORKFLOW_SCRIPT_REF_RE = re.compile(
  r"\b(?:(?:python3|python|bash|sh)\s+|env\s+(?:python3|python|bash|sh)\s+)?"
  r"[\"']?(?:(?:\./|\.\./)*)scripts/([A-Za-z0-9_]+\.(?:py|sh))\b[\"']?"
)


class CiWorkflowScriptRefsError(RuntimeError):
  """Raised when workflow script reference validation cannot complete."""


def fail(message: str) -> None:
  print(f"ci-workflow-script-refs check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise CiWorkflowScriptRefsError(f"workflow {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise CiWorkflowScriptRefsError(f"failed to read workflow {path}: {exc}") from exc


def collect_workflow_script_references(workflow_text: str) -> set[str]:
  try:
    run_text = extract_workflow_run_text(workflow_text)
  except ValueError as exc:
    raise CiWorkflowScriptRefsError(f"failed to parse workflow run steps: {exc}") from exc
  return {match.group(1) for match in WORKFLOW_SCRIPT_REF_RE.finditer(run_text)}


def collect_repo_scripts(scripts_dir: pathlib.Path) -> set[str]:
  scripts: set[str] = set()
  for path in scripts_dir.glob("*"):
    if path.is_file() and path.suffix in (".py", ".sh"):
      scripts.add(path.name)
  return scripts


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate verify.yml only references scripts that exist in scripts/"
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=WORKFLOW_PATH,
    help="Path to workflow yaml",
  )
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=SCRIPTS_DIR,
    help="Path to scripts directory",
  )
  args = parser.parse_args()
  workflow_path = args.workflow.resolve()
  scripts_dir = args.scripts_dir.resolve()

  if not scripts_dir.is_dir():
    fail(f"scripts directory does not exist: {scripts_dir}")

  try:
    workflow_text = read_text(workflow_path)
    workflow_refs = collect_workflow_script_references(workflow_text)
    repo_scripts = collect_repo_scripts(scripts_dir)
  except CiWorkflowScriptRefsError as exc:
    fail(str(exc))

  missing = sorted(workflow_refs - repo_scripts)
  if missing:
    fail("workflow references missing scripts: " + ", ".join(missing))

  print(
    "ci-workflow-script-refs: "
    f"workflow_refs={len(workflow_refs)} repo_scripts={len(repo_scripts)}"
  )
  print("ci-workflow-script-refs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
