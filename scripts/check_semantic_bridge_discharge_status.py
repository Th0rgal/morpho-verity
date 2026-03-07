#!/usr/bin/env python3
"""Fail-closed sync check for upstream-bridge status in SemanticBridgeDischarge.lean."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
DISCHARGE_PATH = ROOT / "Morpho" / "Proofs" / "SemanticBridgeDischarge.lean"

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
]


class SemanticBridgeDischargeStatusError(RuntimeError):
  pass


def normalize_text(text: str) -> str:
  return re.sub(r"\s+", " ", text.replace("`", "")).strip()


def validate_status(text: str) -> None:
  normalized_text = normalize_text(text)
  for expected in (EXPECTED_DISCHARGE_STATUS, EXPECTED_FLASHLOAN_ROW):
    if normalize_text(expected) not in normalized_text:
      raise SemanticBridgeDischargeStatusError(
        "SemanticBridgeDischarge.lean status drift: missing expected text "
        f"`{expected}`"
      )

  stale = [snippet for snippet in FORBIDDEN_SNIPPETS if normalize_text(snippet) in normalized_text]
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
