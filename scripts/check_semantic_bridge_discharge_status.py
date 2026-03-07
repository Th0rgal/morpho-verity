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


class SemanticBridgeDischargeStatusError(RuntimeError):
  pass


def normalize_text(text: str) -> str:
  return re.sub(r"\s+", " ", text.replace("`", "")).strip()


def extract_discharge_section(text: str) -> str:
  return extract_section(
    text=text,
    start_marker=DISCHARGE_SECTION_HEADER,
    end_pattern=r"(?m)^\s*-/\s*$",
    missing_error="SemanticBridgeDischarge.lean status drift: missing `## Discharge Status` section",
    empty_error="SemanticBridgeDischarge.lean status drift: empty `## Discharge Status` section",
  )


def extract_architecture_section(text: str) -> str:
  return extract_section(
    text=text,
    start_marker=ARCHITECTURE_SECTION_HEADER,
    end_marker=PROOF_STRATEGY_SECTION_HEADER,
    missing_error="SemanticBridgeDischarge.lean status drift: missing `## Architecture` section",
    empty_error="SemanticBridgeDischarge.lean status drift: empty `## Architecture` section",
  )


def extract_section(
  *,
  text: str,
  start_marker: str,
  missing_error: str,
  empty_error: str,
  end_marker: str | None = None,
  end_pattern: str | None = None,
) -> str:
  try:
    _, after_start = text.split(start_marker, 1)
  except ValueError as exc:
    raise SemanticBridgeDischargeStatusError(missing_error) from exc

  if end_marker is not None:
    section = after_start.split(end_marker, 1)[0]
  elif end_pattern is not None:
    match = re.search(end_pattern, after_start)
    section = after_start[: match.start()] if match else after_start
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
