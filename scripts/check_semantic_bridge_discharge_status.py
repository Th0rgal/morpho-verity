#!/usr/bin/env python3
"""Fail-closed sync check for upstream-bridge status in SemanticBridgeDischarge.lean."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
DISCHARGE_PATH = ROOT / "Morpho" / "Proofs" / "SemanticBridgeDischarge.lean"

EXPECTED_ARCHITECTURE_SUMMARY = (
  "This file proves Link 1 for `setOwner`, `setFeeRecipient`, `enableIrm`,\n"
  "`enableLltv`, `setAuthorization`, and `flashLoan`.\n"
  "Links 2+3 are already provided upstream for the supported fragment "
  "(verity#1060 / #1065).\n"
  "The remaining blockers here are Link 1 discharge and macro frontend coverage\n"
  "for complex Morpho operations.\n"
  "Verity pin: ad03fc64 (including the two-storage-address witness needed by "
  "`setFeeRecipient`)."
)
EXPECTED_DISCHARGE_STATUS = (
  "With Link 1 proven for 6 operations, the `*SemEq` obligations can be instantiated\n"
  "using the EDSL-based wrappers. For the supported fragment, Links 2+3 are already\n"
  "provided upstream from EDSL execution through compiled IR to EVMYulLean. The\n"
  "remaining repo-local gaps are `flashLoan`'s dynamic-topic `rawLog` witness plus\n"
  "macro/frontend unblockings for the unsupported operations."
)
EXPECTED_FLASHLOAN_ROW = (
  "| 3 | flashLoan | **proven** | pending `SupportedStmtList` witness for the `rawLog` "
  "tail with caller/token topics, then external I/O bridge work |"
)
FORBIDDEN_SNIPPETS = [
  "The remaining gap (Links 2+3) connects the EDSL",
  "execution to the compiled IR and then to EVMYulLean.",
  "Links 2+3 are provided upstream for the supported fragment (verity#1060 / #1065).",
]
ARCHITECTURE_SECTION_HEADER = "## Architecture"
DISCHARGE_SECTION_HEADER = "/-! ## Discharge Status"
PROOF_STRATEGY_SECTION_HEADER = "## Proof Strategy"
PROOF_STRATEGY_SECTION_PATTERN = r"(?m)^\s*## Proof Strategy\s*$"
NAMESPACE_HEADER = "namespace Morpho.Proofs.SemanticBridgeDischarge"
NAMESPACE_FOOTER = "end Morpho.Proofs.SemanticBridgeDischarge"


class SemanticBridgeDischargeStatusError(RuntimeError):
  pass


def normalize_text(text: str) -> str:
  return re.sub(r"\s+", " ", text.replace("`", "")).strip()


def require_unique_line(text: str, line: str, error: str) -> re.Match[str]:
  matches = list(re.finditer(rf"(?m)^\s*{re.escape(line)}\r?$", text))
  if len(matches) != 1:
    raise SemanticBridgeDischargeStatusError(error)
  return matches[0]


def extract_namespace_block(text: str) -> str:
  namespace_match = require_unique_line(
    text,
    NAMESPACE_HEADER,
    "SemanticBridgeDischarge.lean status drift: missing unique namespace header",
  )
  end_match = require_unique_line(
    text,
    NAMESPACE_FOOTER,
    "SemanticBridgeDischarge.lean status drift: missing unique namespace footer",
  )
  if end_match.start() <= namespace_match.end():
    raise SemanticBridgeDischargeStatusError(
      "SemanticBridgeDischarge.lean status drift: invalid namespace boundary ordering"
    )
  return text[namespace_match.end():end_match.start()]


def extract_intro_block(text: str) -> str:
  namespace_match = require_unique_line(
    text,
    NAMESPACE_HEADER,
    "SemanticBridgeDischarge.lean status drift: missing unique namespace header",
  )
  intro = text[:namespace_match.start()]
  if not intro.strip():
    raise SemanticBridgeDischargeStatusError(
      "SemanticBridgeDischarge.lean status drift: empty intro block before namespace"
    )
  return intro


def extract_discharge_section(text: str) -> str:
  return extract_section(
    text=extract_namespace_block(text),
    start_marker=DISCHARGE_SECTION_HEADER,
    end_pattern=r"(?m)^\s*-/\s*$",
    missing_error="SemanticBridgeDischarge.lean status drift: missing `## Discharge Status` section",
    empty_error="SemanticBridgeDischarge.lean status drift: empty `## Discharge Status` section",
    missing_end_error="SemanticBridgeDischarge.lean status drift: missing closing `-/` for `## Discharge Status` section",
  )


def extract_architecture_section(text: str) -> str:
  return extract_section(
    text=extract_intro_block(text),
    start_marker=ARCHITECTURE_SECTION_HEADER,
    end_pattern=PROOF_STRATEGY_SECTION_PATTERN,
    missing_error="SemanticBridgeDischarge.lean status drift: missing `## Architecture` section",
    empty_error="SemanticBridgeDischarge.lean status drift: empty `## Architecture` section",
    missing_end_error="SemanticBridgeDischarge.lean status drift: missing `## Proof Strategy` boundary after `## Architecture` section",
  )


def extract_section(
  *,
  text: str,
  start_marker: str,
  missing_error: str,
  empty_error: str,
  missing_end_error: str,
  end_marker: str | None = None,
  end_pattern: str | None = None,
) -> str:
  start_match = require_unique_line(text, start_marker, missing_error)
  after_start = text[start_match.end():]

  if end_marker is not None:
    end_match = require_unique_line(after_start, end_marker, missing_end_error)
    section = after_start[:end_match.start()]
  elif end_pattern is not None:
    match = re.search(end_pattern, after_start)
    if match is None:
      raise SemanticBridgeDischargeStatusError(missing_end_error)
    section = after_start[: match.start()]
  else:
    section = after_start

  if not section.strip():
    raise SemanticBridgeDischargeStatusError(empty_error)
  return section


def validate_status(text: str) -> None:
  architecture_text = normalize_text(extract_architecture_section(text))
  discharge_text = normalize_text(extract_discharge_section(text))

  expected_sections = (
    (architecture_text, EXPECTED_ARCHITECTURE_SUMMARY),
    (discharge_text, EXPECTED_DISCHARGE_STATUS),
    (discharge_text, EXPECTED_FLASHLOAN_ROW),
  )
  for section_text, expected in expected_sections:
    if normalize_text(expected) not in section_text:
      raise SemanticBridgeDischargeStatusError(
        "SemanticBridgeDischarge.lean status drift: missing expected text "
        f"`{expected}`"
      )

  combined_text = architecture_text + " " + discharge_text
  stale = [snippet for snippet in FORBIDDEN_SNIPPETS if normalize_text(snippet) in combined_text]
  if stale:
    raise SemanticBridgeDischargeStatusError(
      "SemanticBridgeDischarge.lean still contains stale bridge status text: "
      + "; ".join(stale)
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate upstream semantic-bridge status in SemanticBridgeDischarge.lean"
  )
  parser.add_argument("--discharge", type=pathlib.Path, default=DISCHARGE_PATH)
  args = parser.parse_args()

  text = args.discharge.read_text(encoding="utf-8")
  validate_status(text)

  print("semantic-bridge-discharge-status check: OK")
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except SemanticBridgeDischargeStatusError as e:
    print(f"semantic-bridge-discharge-status check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
  except FileNotFoundError as e:
    print(f"semantic-bridge-discharge-status check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
