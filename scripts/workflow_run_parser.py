#!/usr/bin/env python3
"""Utilities for extracting executable `run:` command text from CI workflow YAML."""

from __future__ import annotations

import re

RUN_STEP_RE = re.compile(r"^(\s*)run:\s*(.*)$")
RUN_BLOCK_SCALAR_RE = re.compile(r"^[|>][-+]?$")


def extract_workflow_run_text(workflow_text: str) -> str:
  """Return concatenated command text from workflow `run:` steps only."""
  commands: list[str] = []
  lines = workflow_text.splitlines()
  i = 0
  while i < len(lines):
    line = lines[i]
    match = RUN_STEP_RE.match(line)
    if match is None:
      i += 1
      continue

    run_indent = len(match.group(1))
    tail = match.group(2).strip()
    if tail and not RUN_BLOCK_SCALAR_RE.fullmatch(tail):
      line_parts = [tail]
      i += 1
      while i < len(lines):
        candidate = lines[i]
        stripped = candidate.lstrip(" ")
        if not stripped:
          break
        indent = len(candidate) - len(stripped)
        if indent <= run_indent:
          break
        line_parts.append(stripped)
        i += 1
      commands.append("\n".join(line_parts))
      continue

    i += 1
    block_lines: list[str] = []
    while i < len(lines):
      candidate = lines[i]
      stripped = candidate.lstrip(" ")
      if stripped:
        indent = len(candidate) - len(stripped)
        if indent <= run_indent:
          break
        block_lines.append(stripped)
      else:
        block_lines.append("")
      i += 1
    commands.append("\n".join(block_lines))
  return "\n".join(commands)
