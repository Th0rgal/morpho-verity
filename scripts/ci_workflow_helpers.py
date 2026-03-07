#!/usr/bin/env python3
"""Shared helpers for verify.yml integrity checks."""

from __future__ import annotations

import pathlib
import re


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"

DOUBLE_QUOTED_ESCAPES = {
  "0": "\0",
  "a": "\a",
  "b": "\b",
  "t": "\t",
  "n": "\n",
  "v": "\v",
  "f": "\f",
  "r": "\r",
  "e": "\x1b",
  " ": " ",
  '"': '"',
  "/": "/",
  "\\": "\\",
  "N": "\x85",
  "_": "\xa0",
  "L": "\u2028",
  "P": "\u2029",
}


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
    elif (
      char == "#"
      and not in_single_quote
      and not in_double_quote
      and (index == 0 or value[index - 1].isspace())
    ):
      return value[:index].rstrip()
    index += 1
  return value.rstrip()


def strip_yaml_scalar(value: str) -> str:
  value = strip_yaml_comment(value).strip()
  if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
    inner = value[1:-1]
    if value[0] == "'":
      return inner.replace("''", "'")
    parsed = _parse_double_quoted_yaml_scalar(inner)
    if parsed is not None:
      return parsed
  return value


def _parse_double_quoted_yaml_scalar(inner: str) -> str | None:
  parsed: list[str] = []
  index = 0
  while index < len(inner):
    char = inner[index]
    if char != "\\":
      parsed.append(char)
      index += 1
      continue
    index += 1
    if index >= len(inner):
      return None
    escape = inner[index]
    if escape in DOUBLE_QUOTED_ESCAPES:
      parsed.append(DOUBLE_QUOTED_ESCAPES[escape])
      index += 1
      continue
    if escape in {"x", "u", "U"}:
      width = {"x": 2, "u": 4, "U": 8}[escape]
      digits = inner[index + 1:index + 1 + width]
      if len(digits) != width or re.fullmatch(rf"[0-9A-Fa-f]{{{width}}}", digits) is None:
        return None
      parsed.append(chr(int(digits, 16)))
      index += 1 + width
      continue
    return None
  return "".join(parsed)
