#!/usr/bin/env python3
"""Shared helpers for verify.yml integrity checks."""

from __future__ import annotations

import pathlib


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"


def read_text(path: pathlib.Path, error_cls: type[Exception], *, label: str = "workflow") -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise error_cls(f"{label} {path} is not valid UTF-8: {exc}") from exc
  except OSError as exc:
    raise error_cls(f"failed to read {label} {path}: {exc}") from exc


def strip_yaml_comment(value: str) -> str:
  in_single_quote = False
  in_double_quote = False
  index = 0
  while index < len(value):
    char = value[index]
    if char == "'" and not in_double_quote:
      if in_single_quote and index + 1 < len(value) and value[index + 1] == "'":
        index += 2
        continue
      in_single_quote = not in_single_quote
    elif char == '"' and not in_single_quote:
      backslash_count = 0
      cursor = index - 1
      while cursor >= 0 and value[cursor] == "\\":
        backslash_count += 1
        cursor -= 1
      if backslash_count % 2 == 0:
        in_double_quote = not in_double_quote
    elif char == "#" and not in_single_quote and not in_double_quote:
      return value[:index].rstrip()
    index += 1
  return value.rstrip()


def strip_yaml_scalar(value: str) -> str:
  value = strip_yaml_comment(value).strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    return value[1:-1]
  return value
