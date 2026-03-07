#!/usr/bin/env python3
"""Fail-closed sync check for the semantic-bridge status summary in README.md."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from check_semantic_bridge_readiness_summary import derive_summary
from check_semantic_bridge_readiness_sync import (
  SemanticBridgeReadinessSyncError,
  load_config,
)


ROOT = pathlib.Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
README_PATH = ROOT / "README.md"

PROVEN_RE = re.compile(
  r"\*\*Link 1 proofs \(stable `Morpho\.\*` wrapper API ↔ EDSL\) are now proven for "
  r"(?P<proven>\d+)/(?P<total>\d+) operations:\*\*"
)
ASSUMED_RE = re.compile(
  r"The remaining (?P<assumed>\d+)/(?P<total>\d+) operations still have assumed Link 1 status in\s+"
  r"`config/semantic-bridge-obligations\.json`\."
)
UPSTREAM_STATUS_PREFIX = (
  "The Verity framework now has the upstream typed-IR / canonical-semantics bridge\n"
  "for supported `verity_contract` functions:\n"
  "`EDSL execution ≡ EVMYulLean(compile(CompilationModel))`."
)
SECTION_HEADER_PATTERN = re.compile(
  r"(?m)^\s*### Upstream: Verity hybrid canonical-semantics migration \(verity#1060 / #1065\)\s*$"
)
SECTION_END_PATTERN = re.compile(r"(?m)^\s*Machine-readable parity target artifacts:\s*$")


class ReadmeSemanticBridgeSummaryError(RuntimeError):
  pass


def read_text(path: pathlib.Path, *, context: str) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise ReadmeSemanticBridgeSummaryError(f"failed to read {context} {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise ReadmeSemanticBridgeSummaryError(
      f"failed to decode {context} {path} as UTF-8: {exc}"
    ) from exc


def load_summary(path: pathlib.Path) -> dict[str, object]:
  try:
    return derive_summary(load_config(path))
  except SemanticBridgeReadinessSyncError as exc:
    raise ReadmeSemanticBridgeSummaryError(str(exc)) from exc


def normalize_text(text: str) -> str:
  return re.sub(r"\s+", " ", text.replace("`", "")).strip()


def require_match(pattern: re.Pattern[str], text: str, description: str) -> re.Match[str]:
  match = pattern.search(text)
  if match is None:
    raise ReadmeSemanticBridgeSummaryError(f"failed to locate {description} in README.md")
  return match


def require_unique_match(pattern: re.Pattern[str], text: str, description: str) -> re.Match[str]:
  matches = list(pattern.finditer(text))
  if not matches:
    raise ReadmeSemanticBridgeSummaryError(f"failed to locate {description} in README.md")
  if len(matches) > 1:
    raise ReadmeSemanticBridgeSummaryError(f"found multiple {description} matches in README.md")
  return matches[0]


def extract_semantic_bridge_section(text: str) -> str:
  section_start = require_unique_match(
    SECTION_HEADER_PATTERN,
    text,
    "semantic-bridge section header",
  )
  section_end = require_unique_match(
    SECTION_END_PATTERN,
    text,
    "semantic-bridge section end boundary",
  )
  if section_end.start() <= section_start.end():
    raise ReadmeSemanticBridgeSummaryError(
      "README semantic-bridge section has an invalid boundary ordering"
    )
  section = text[section_start.end() : section_end.start()]
  if not section.strip():
    raise ReadmeSemanticBridgeSummaryError("README semantic-bridge section is empty")
  return section


def extract_link1_operations(section_text: str) -> list[str]:
  summary_match = require_unique_match(PROVEN_RE, section_text, "Link 1 proof summary")
  block_start = summary_match.end()
  assumed_match = require_unique_match(ASSUMED_RE, section_text, "assumed Link 1 summary")
  block_end = assumed_match.start()
  if block_end <= block_start:
    raise ReadmeSemanticBridgeSummaryError(
      "README Link 1 operation list has an invalid boundary ordering"
    )
  ops_block = section_text[block_start:block_end]
  raw_items = [item.strip() for item in ops_block.replace("\n", " ").split(",")]
  operations: list[str] = []
  none_count = 0
  for item in raw_items:
    if not item:
      continue
    if item.lower() == "none":
      none_count += 1
      continue
    operation_match = re.fullmatch(r"`([^`]+)`\.?", item)
    if operation_match is None:
      raise ReadmeSemanticBridgeSummaryError(
        "README Link 1 operation list contains malformed entries"
      )
    operations.append(operation_match.group(1))
  if none_count > 1:
    raise ReadmeSemanticBridgeSummaryError(
      "README Link 1 operation list repeats the none sentinel"
    )
  if none_count and operations:
    raise ReadmeSemanticBridgeSummaryError(
      "README Link 1 operation list mixes the none sentinel with named operations"
    )
  if none_count:
    return []
  if not operations:
    raise ReadmeSemanticBridgeSummaryError("README Link 1 operation list is empty")
  if len(operations) != len(set(operations)):
    raise ReadmeSemanticBridgeSummaryError("README Link 1 operation list contains duplicate operations")
  return operations


def validate_summary(text: str, summary: dict[str, object]) -> None:
  section_text = extract_semantic_bridge_section(text)
  normalized_section_text = normalize_text(section_text)

  if normalize_text(UPSTREAM_STATUS_PREFIX) not in normalized_section_text:
    raise ReadmeSemanticBridgeSummaryError(
      "README upstream semantic-bridge status drift: "
      f"expected `{UPSTREAM_STATUS_PREFIX}`"
    )

  proven_match = require_unique_match(PROVEN_RE, section_text, "Link 1 proof summary")
  proven_count = int(proven_match.group("proven"))
  total_count = int(proven_match.group("total"))
  expected_proven = int(summary["link1_count"])
  expected_total = int(summary["total"])
  if proven_count != expected_proven or total_count != expected_total:
    raise ReadmeSemanticBridgeSummaryError(
      "README Link 1 proof summary drift: "
      f"expected {expected_proven}/{expected_total}, found {proven_count}/{total_count}"
    )

  actual_operations = extract_link1_operations(section_text)
  expected_operations = list(summary["link1_operations"])
  if not expected_operations and actual_operations:
    raise ReadmeSemanticBridgeSummaryError(
      "README Link 1 operation list drift: expected no operations; found "
      + ", ".join(actual_operations)
    )
  if set(actual_operations) != set(expected_operations):
    raise ReadmeSemanticBridgeSummaryError(
      "README Link 1 operation list drift: expected "
      + ", ".join(expected_operations)
      + "; found "
      + ", ".join(actual_operations)
    )

  assumed_match = require_unique_match(ASSUMED_RE, section_text, "assumed Link 1 summary")
  assumed_count = int(assumed_match.group("assumed"))
  assumed_total = int(assumed_match.group("total"))
  expected_assumed = int(summary["assumed_count"])
  if assumed_count != expected_assumed or assumed_total != expected_total:
    raise ReadmeSemanticBridgeSummaryError(
      "README assumed Link 1 summary drift: "
      f"expected {expected_assumed}/{expected_total}, found {assumed_count}/{assumed_total}"
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate the semantic-bridge status summary in README.md"
  )
  parser.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
  parser.add_argument("--readme", type=pathlib.Path, default=README_PATH)
  args = parser.parse_args()

  summary = load_summary(args.config)
  text = read_text(args.readme, context="README file")
  validate_summary(text, summary)

  print("readme-semantic-bridge-summary check: OK")
  print(
    f"counts: total={summary['total']} link1={summary['link1_count']} assumed={summary['assumed_count']}"
  )
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except ReadmeSemanticBridgeSummaryError as e:
    print(f"readme-semantic-bridge-summary check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
