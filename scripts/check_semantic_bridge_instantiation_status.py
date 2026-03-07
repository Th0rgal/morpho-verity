#!/usr/bin/env python3
"""Fail-closed sync check for upstream-bridge status in SemanticBridgeInstantiation.lean."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
INSTANTIATION_PATH = ROOT / "Morpho" / "Proofs" / "SemanticBridgeInstantiation.lean"

EXPECTED_INTRO_STATUS = (
  "Links 2+3 are already provided upstream for the supported fragment, so for the\n"
  "operations instantiated here the remaining repo-local work is Link 1 discharge\n"
  "plus macro/frontend unblockings on the unsupported surface. Composing these\n"
  "theorems with the upstream typed-IR/compiler results extends them to the\n"
  "compiled on-chain Morpho contract for the supported operations."
)
EXPECTED_SUMMARY_HEADING = "### Upstream bridge composition"
EXPECTED_SUMMARY_STATUS = (
  "The supported-fragment semantic bridge already has:"
)
EXPECTED_SUMMARY_COMPOSITION = (
  "Composing those upstream results with these 21 instantiation theorems yields\n"
  "compiled-bytecode invariant preservation for the operations above. The\n"
  "remaining repo-local work is proving Link 1 and completing macro/frontend\n"
  "migration for the other Morpho operations."
)
FORBIDDEN_SNIPPETS = [
  "The remaining gap (Links 2+3) will eventually show that the compiled EVM",
  "The full semantic bridge additionally requires:",
  "Once Links 2+3 are composed, these 21 theorems extend to the compiled",
]


class SemanticBridgeInstantiationStatusError(RuntimeError):
  pass


def read_instantiation_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except (OSError, UnicodeDecodeError) as exc:
    raise SemanticBridgeInstantiationStatusError(
      f"failed to read SemanticBridgeInstantiation source `{path}`: {exc}"
    ) from exc


def normalize_text(text: str) -> str:
  return re.sub(r"\s+", " ", text.replace("`", "")).strip()


VALIDATES_SECTION_HEADER = "## What this validates"
SUMMARY_SECTION_HEADER = "/-! ## Summary"
NAMESPACE_HEADER = "namespace Morpho.Proofs.SemanticBridgeInstantiation"
NAMESPACE_FOOTER = "end Morpho.Proofs.SemanticBridgeInstantiation"
CLOSING_DOCBLOCK_PATTERN = r"(?m)^\s*-/\s*$"
VALIDATES_SECTION_PATTERN = r"(?m)^\s*## What this validates\s*$"
SUMMARY_SECTION_PATTERN = r"(?m)^\s*/-!\s*## Summary\s*$"
TRACKED_NAMESPACE_PATTERNS = (
  re.compile(SUMMARY_SECTION_PATTERN),
  re.compile(re.escape(EXPECTED_SUMMARY_STATUS), re.DOTALL),
  re.compile(re.escape(EXPECTED_SUMMARY_COMPOSITION), re.DOTALL),
)


def require_unique_match(
  *,
  text: str,
  pattern: str,
  missing_error: str,
  duplicate_error: str,
) -> re.Match[str]:
  matches = list(re.finditer(pattern, text))
  if not matches:
    raise SemanticBridgeInstantiationStatusError(missing_error)
  if len(matches) > 1:
    raise SemanticBridgeInstantiationStatusError(duplicate_error)
  return matches[0]


def extract_namespace_blocks(text: str) -> list[tuple[re.Match[str], re.Match[str], str]]:
  namespace_matches = list(re.finditer(rf"(?m)^\s*{re.escape(NAMESPACE_HEADER)}\r?$", text))
  if not namespace_matches:
    raise SemanticBridgeInstantiationStatusError(
      "SemanticBridgeInstantiation.lean status drift: missing namespace boundary after `## What this validates` section"
    )
  end_matches = list(re.finditer(rf"(?m)^\s*{re.escape(NAMESPACE_FOOTER)}\r?$", text))
  if not end_matches:
    raise SemanticBridgeInstantiationStatusError(
      "SemanticBridgeInstantiation.lean status drift: missing namespace footer"
    )
  if end_matches[0].start() < namespace_matches[0].start():
    raise SemanticBridgeInstantiationStatusError(
      "SemanticBridgeInstantiation.lean status drift: unmatched namespace footer before first namespace block"
    )
  blocks: list[tuple[re.Match[str], re.Match[str], str]] = []
  end_index = 0
  previous_end = -1
  for namespace_match in namespace_matches:
    if namespace_match.start() < previous_end:
      raise SemanticBridgeInstantiationStatusError(
        "SemanticBridgeInstantiation.lean status drift: namespace blocks overlap"
      )
    while end_index < len(end_matches) and end_matches[end_index].start() <= namespace_match.end():
      end_index += 1
    if end_index >= len(end_matches):
      raise SemanticBridgeInstantiationStatusError(
        "SemanticBridgeInstantiation.lean status drift: missing namespace footer"
      )
    end_match = end_matches[end_index]
    namespace_body = text[namespace_match.end() : end_match.start()]
    if not namespace_body.strip():
      raise SemanticBridgeInstantiationStatusError(
        "SemanticBridgeInstantiation.lean status drift: empty namespace body"
      )
    blocks.append((namespace_match, end_match, namespace_body))
    previous_end = end_match.end()
    end_index += 1
  if end_index != len(end_matches):
    raise SemanticBridgeInstantiationStatusError(
      "SemanticBridgeInstantiation.lean status drift: unmatched namespace footer after namespace blocks"
    )
  return blocks


def namespace_block_has_tracked_status(namespace_body: str) -> bool:
  return any(pattern.search(namespace_body) is not None for pattern in TRACKED_NAMESPACE_PATTERNS)


def extract_namespace_block(text: str) -> str:
  namespace_blocks = extract_namespace_blocks(text)
  _, _, primary_namespace_body = namespace_blocks[0]
  if not namespace_block_has_tracked_status(primary_namespace_body):
    raise SemanticBridgeInstantiationStatusError(
      "SemanticBridgeInstantiation.lean status drift: primary namespace block is missing tracked status content"
    )
  duplicate_status_blocks = [
    match.group(0).strip()
    for match, _, namespace_body in namespace_blocks[1:]
    if namespace_block_has_tracked_status(namespace_body)
  ]
  if duplicate_status_blocks:
    raise SemanticBridgeInstantiationStatusError(
      "SemanticBridgeInstantiation.lean status drift: found multiple namespace blocks with tracked status content"
    )
  return primary_namespace_body


def extract_intro_block(text: str) -> str:
  namespace_match = extract_namespace_blocks(text)[0][0]
  intro = text[:namespace_match.start()]
  if not intro.strip():
    raise SemanticBridgeInstantiationStatusError(
      "SemanticBridgeInstantiation.lean status drift: empty intro block before namespace"
    )
  return intro


def extract_section(
  *,
  text: str,
  start_pattern: str,
  start_label: str,
  missing_error: str,
  empty_error: str,
  missing_end_error: str,
  end_marker: str | None = None,
  end_pattern: str | None = None,
) -> str:
  start_match = require_unique_match(
    text=text,
    pattern=start_pattern,
    missing_error=missing_error,
    duplicate_error=f"SemanticBridgeInstantiation.lean status drift: multiple `{start_label}` section markers",
  )

  after_start = text[start_match.end() :]

  if end_marker is not None:
    try:
      section, _ = after_start.split(end_marker, 1)
    except ValueError as exc:
      raise SemanticBridgeInstantiationStatusError(missing_end_error) from exc
  elif end_pattern is not None:
    match = re.search(end_pattern, after_start)
    if match is None:
      raise SemanticBridgeInstantiationStatusError(missing_end_error)
    section = after_start[: match.start()]
  else:
    section = after_start

  if not section.strip():
    raise SemanticBridgeInstantiationStatusError(empty_error)
  return section


def extract_validates_section(text: str) -> str:
  return extract_section(
    text=extract_intro_block(text),
    start_pattern=VALIDATES_SECTION_PATTERN,
    start_label=VALIDATES_SECTION_HEADER,
    missing_error="SemanticBridgeInstantiation.lean status drift: missing `## What this validates` section",
    empty_error="SemanticBridgeInstantiation.lean status drift: empty `## What this validates` section",
    missing_end_error="SemanticBridgeInstantiation.lean status drift: missing closing `-/` for `## What this validates` section",
    end_pattern=CLOSING_DOCBLOCK_PATTERN,
  )


def extract_summary_section(text: str) -> str:
  return extract_section(
    text=extract_namespace_block(text),
    start_pattern=SUMMARY_SECTION_PATTERN,
    start_label=SUMMARY_SECTION_HEADER,
    end_pattern=CLOSING_DOCBLOCK_PATTERN,
    missing_error="SemanticBridgeInstantiation.lean status drift: missing `## Summary` section",
    empty_error="SemanticBridgeInstantiation.lean status drift: empty `## Summary` section",
    missing_end_error="SemanticBridgeInstantiation.lean status drift: missing closing `-/` for `## Summary` section",
  )


def validate_status(text: str) -> None:
  validates_text = normalize_text(extract_validates_section(text))
  summary_text = normalize_text(extract_summary_section(text))

  expected_sections = (
    (validates_text, EXPECTED_INTRO_STATUS),
    (summary_text, EXPECTED_SUMMARY_HEADING),
    (summary_text, EXPECTED_SUMMARY_STATUS),
    (summary_text, EXPECTED_SUMMARY_COMPOSITION),
  )
  for section_text, expected in expected_sections:
    if normalize_text(expected) not in section_text:
      raise SemanticBridgeInstantiationStatusError(
        "SemanticBridgeInstantiation.lean status drift: missing expected text "
        f"`{expected}`"
      )

  combined_text = validates_text + " " + summary_text
  stale = [snippet for snippet in FORBIDDEN_SNIPPETS if normalize_text(snippet) in combined_text]
  if stale:
    raise SemanticBridgeInstantiationStatusError(
      "SemanticBridgeInstantiation.lean still contains stale future-tense bridge text: "
      + "; ".join(stale)
    )



def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate upstream semantic-bridge status in SemanticBridgeInstantiation.lean"
  )
  parser.add_argument("--instantiation", type=pathlib.Path, default=INSTANTIATION_PATH)
  args = parser.parse_args()
  instantiation_path = args.instantiation.resolve()

  text = read_instantiation_text(instantiation_path)
  validate_status(text)

  print("semantic-bridge-instantiation-status check: OK")
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except SemanticBridgeInstantiationStatusError as e:
    print(f"semantic-bridge-instantiation-status check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
