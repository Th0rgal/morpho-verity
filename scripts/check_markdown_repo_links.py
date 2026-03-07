#!/usr/bin/env python3
"""Fail-closed check that repo-local markdown links resolve to existing files."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys
from urllib.parse import urlsplit


ROOT = pathlib.Path(__file__).resolve().parent.parent
MARKDOWN_LINK_RE = re.compile(r"!?(?:\[[^\]]*\])\(([^)]+)\)")
MARKDOWN_LINK_TITLE_RE = re.compile(
  r"""^(?P<path>\S+?)(?:\s+(?:"[^"]*"|'[^']*'|\([^)]+\)))?\s*$"""
)


class MarkdownRepoLinksError(RuntimeError):
  """Raised when markdown repo-link validation cannot complete safely."""


def fail(message: str) -> None:
  print(f"markdown-repo-links check failed: {message}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except UnicodeDecodeError as exc:
    raise MarkdownRepoLinksError(f"failed to decode markdown file {path} as UTF-8: {exc}") from exc
  except OSError as exc:
    raise MarkdownRepoLinksError(f"failed to read markdown file {path}: {exc}") from exc


def display_path(path: pathlib.Path, *, root: pathlib.Path) -> pathlib.Path:
  try:
    return path.relative_to(root)
  except ValueError:
    return path


def collect_default_docs(root: pathlib.Path) -> list[pathlib.Path]:
  docs: list[pathlib.Path] = []
  readme_path = root / "README.md"
  if readme_path.is_file():
    docs.append(readme_path)
  docs_dir = root / "docs"
  if docs_dir.is_dir():
    docs.extend(sorted(path for path in docs_dir.rglob("*.md") if path.is_file()))
  return docs


def normalize_link_target(raw_target: str) -> str:
  target = raw_target.strip()
  if target.startswith("<") and target.endswith(">"):
    target = target[1:-1].strip()
  return target


def extract_link_path_text(target: str) -> str:
  match = MARKDOWN_LINK_TITLE_RE.fullmatch(target)
  if match is not None:
    return match.group("path")
  return target


def is_external_target(target: str) -> bool:
  if target.startswith("//"):
    return True
  parsed = urlsplit(target)
  return bool(parsed.scheme)


def resolve_repo_link(doc_path: pathlib.Path, target: str, *, root: pathlib.Path) -> pathlib.Path | None:
  normalized = normalize_link_target(target)
  if not normalized or normalized.startswith("#") or is_external_target(normalized):
    return None
  path_text = extract_link_path_text(normalized).split("#", 1)[0]
  if not path_text:
    return None
  candidate = (doc_path.parent / path_text).resolve()
  try:
    candidate.relative_to(root)
  except ValueError as exc:
    raise MarkdownRepoLinksError(
      f"{display_path(doc_path, root=root)} links outside repo root: {target}"
    ) from exc
  return candidate


def collect_missing_repo_links(doc_path: pathlib.Path, *, root: pathlib.Path) -> list[str]:
  text = read_text(doc_path)
  missing: list[str] = []
  for match in MARKDOWN_LINK_RE.finditer(text):
    target = match.group(1)
    resolved = resolve_repo_link(doc_path, target, root=root)
    if resolved is None:
      continue
    if not resolved.exists():
      missing.append(target)
  return missing


def main(argv: list[str] | None = None) -> int:
  parser = argparse.ArgumentParser(
    description="Validate repo-local markdown links in README.md and docs/*.md"
  )
  parser.add_argument(
    "--root",
    type=pathlib.Path,
    default=ROOT,
    help="Repository root used for default markdown doc discovery",
  )
  parser.add_argument(
    "--doc",
    action="append",
    default=[],
    help="Markdown file to validate explicitly (may be repeated)",
  )
  args = parser.parse_args(argv)

  root = args.root.resolve()
  doc_paths = [pathlib.Path(raw).resolve() for raw in args.doc] if args.doc else collect_default_docs(root)
  if not doc_paths:
    fail(f"no markdown docs found under {root}")

  missing_by_doc: list[str] = []
  for doc_path in doc_paths:
    if not doc_path.is_file():
      fail(f"markdown file does not exist: {doc_path}")
    try:
      doc_path.relative_to(root)
    except ValueError:
      fail(f"markdown file is outside repo root {root}: {doc_path}")
    missing_targets = collect_missing_repo_links(doc_path, root=root)
    if not missing_targets:
      continue
    missing_by_doc.append(
      f"{display_path(doc_path, root=root)} -> " + ", ".join(sorted(missing_targets))
    )

  if missing_by_doc:
    fail("missing repo-local markdown links: " + " | ".join(missing_by_doc))

  print(f"markdown-repo-links: docs={len(doc_paths)} root={root}")
  print("markdown-repo-links check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
