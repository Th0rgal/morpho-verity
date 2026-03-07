#!/usr/bin/env python3
"""Fail-closed sync check for the semantic-bridge status summary in README.md."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from check_semantic_bridge_readiness_summary import derive_summary
from check_semantic_bridge_readiness_sync import load_config


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


class ReadmeSemanticBridgeSummaryError(RuntimeError):
  pass


def require_match(pattern: re.Pattern[str], text: str, description: str) -> re.Match[str]:
  match = pattern.search(text)
  if match is None:
    raise ReadmeSemanticBridgeSummaryError(f"failed to locate {description} in README.md")
  return match


def extract_link1_operations(text: str) -> list[str]:
  summary_match = require_match(PROVEN_RE, text, "Link 1 proof summary")
  block_start = summary_match.end()
  assumed_match = require_match(ASSUMED_RE, text, "assumed Link 1 summary")
  block_end = assumed_match.start()
  if block_end == -1:
    raise ReadmeSemanticBridgeSummaryError("failed to locate assumed Link 1 summary in README.md")

  ops_block = text[block_start:block_end]
  operations = re.findall(r"`([^`]+)`", ops_block)
  has_none_sentinel = re.search(r"\bnone\b", ops_block, re.IGNORECASE) is not None
  if has_none_sentinel and operations:
    raise ReadmeSemanticBridgeSummaryError(
      "README Link 1 operation list mixes the none sentinel with named operations"
    )
  if has_none_sentinel:
    return []
  if not operations:
    raise ReadmeSemanticBridgeSummaryError("README Link 1 operation list is empty")
  if len(operations) != len(set(operations)):
    raise ReadmeSemanticBridgeSummaryError("README Link 1 operation list contains duplicate operations")
  return operations


def validate_summary(text: str, summary: dict[str, object]) -> None:
  if UPSTREAM_STATUS_PREFIX not in text:
    raise ReadmeSemanticBridgeSummaryError(
      "README upstream semantic-bridge status drift: "
      f"expected `{UPSTREAM_STATUS_PREFIX}`"
    )

  proven_match = require_match(PROVEN_RE, text, "Link 1 proof summary")
  proven_count = int(proven_match.group("proven"))
  total_count = int(proven_match.group("total"))
  expected_proven = int(summary["link1_count"])
  expected_total = int(summary["total"])
  if proven_count != expected_proven or total_count != expected_total:
    raise ReadmeSemanticBridgeSummaryError(
      "README Link 1 proof summary drift: "
      f"expected {expected_proven}/{expected_total}, found {proven_count}/{total_count}"
    )

  actual_operations = extract_link1_operations(text)
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

  assumed_match = require_match(ASSUMED_RE, text, "assumed Link 1 summary")
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

  summary = derive_summary(load_config(args.config))
  text = args.readme.read_text(encoding="utf-8")
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
  except FileNotFoundError as e:
    print(f"readme-semantic-bridge-summary check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
