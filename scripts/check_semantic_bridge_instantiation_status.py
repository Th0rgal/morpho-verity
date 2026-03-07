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


def normalize_text(text: str) -> str:
  return re.sub(r"\s+", " ", text.replace("`", "")).strip()


VALIDATES_SECTION_HEADER = "## What this validates"
SUMMARY_SECTION_HEADER = "/-! ## Summary"
NAMESPACE_HEADER = "namespace Morpho.Proofs.SemanticBridgeInstantiation"
CLOSING_DOCBLOCK_PATTERN = r"(?m)^\s*-/\s*$"
VALIDATES_SECTION_PATTERN = r"(?m)^\s*## What this validates\s*$"
SUMMARY_SECTION_PATTERN = r"(?m)^\s*/-!\s*## Summary\s*$"
NAMESPACE_PATTERN = r"(?m)^\s*namespace Morpho\.Proofs\.SemanticBridgeInstantiation\s*$"


def extract_section(
  *,
  text: str,
  start_pattern: str,
  missing_error: str,
  empty_error: str,
  missing_end_error: str,
  end_marker: str | None = None,
  end_pattern: str | None = None,
) -> str:
  start_match = re.search(start_pattern, text)
  if start_match is None:
    raise SemanticBridgeInstantiationStatusError(missing_error)

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
  section = extract_section(
    text=text,
    start_pattern=VALIDATES_SECTION_PATTERN,
    end_pattern=NAMESPACE_PATTERN,
    missing_error="SemanticBridgeInstantiation.lean status drift: missing `## What this validates` section",
    empty_error="SemanticBridgeInstantiation.lean status drift: empty `## What this validates` section",
    missing_end_error="SemanticBridgeInstantiation.lean status drift: missing namespace boundary after `## What this validates` section",
  )
  if not section.strip().endswith("-/"):
    raise SemanticBridgeInstantiationStatusError(
      "SemanticBridgeInstantiation.lean status drift: missing closing `-/` for `## What this validates` section"
    )
  return section


def extract_summary_section(text: str) -> str:
  return extract_section(
    text=text,
    start_pattern=SUMMARY_SECTION_PATTERN,
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

  text = args.instantiation.read_text(encoding="utf-8")
  validate_status(text)

  print("semantic-bridge-instantiation-status check: OK")
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except SemanticBridgeInstantiationStatusError as e:
    print(f"semantic-bridge-instantiation-status check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
  except FileNotFoundError as e:
    print(f"semantic-bridge-instantiation-status check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
