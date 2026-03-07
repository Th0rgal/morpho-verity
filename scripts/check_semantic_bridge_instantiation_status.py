#!/usr/bin/env python3
"""Fail-closed sync check for upstream-bridge status in SemanticBridgeInstantiation.lean."""

from __future__ import annotations

import argparse
import pathlib
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


def validate_status(text: str) -> None:
  for expected in (
    EXPECTED_INTRO_STATUS,
    EXPECTED_SUMMARY_HEADING,
    EXPECTED_SUMMARY_STATUS,
    EXPECTED_SUMMARY_COMPOSITION,
  ):
    if expected not in text:
      raise SemanticBridgeInstantiationStatusError(
        "SemanticBridgeInstantiation.lean status drift: missing expected text "
        f"`{expected}`"
      )

  stale = [snippet for snippet in FORBIDDEN_SNIPPETS if snippet in text]
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
