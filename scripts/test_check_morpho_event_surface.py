#!/usr/bin/env python3
"""Unit tests for Morpho generated event surface sync checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_morpho_event_surface import (  # noqa: E402
  EventParam,
  MorphoEventSurfaceError,
  extract_generated_events,
  extract_solidity_events,
  validate_event_surface,
)


ROOT = pathlib.Path(__file__).resolve().parent.parent


class MorphoEventSurfaceTests(unittest.TestCase):
  def test_extract_solidity_events_maps_id_and_market_params(self) -> None:
    events = extract_solidity_events(
      """
library EventsLib {
  event CreateMarket(Id indexed id, MarketParams marketParams);
  event IncrementNonce(address indexed caller, address indexed authorizer, uint256 usedNonce);
}
"""
    )
    self.assertEqual(
      events["CreateMarket"],
      [
        EventParam("id", "bytes32", True),
        EventParam("marketParams", "tuple(address,address,address,address,uint256)", False),
      ],
    )
    self.assertEqual(
      events["IncrementNonce"],
      [
        EventParam("caller", "address", True),
        EventParam("authorizer", "address", True),
        EventParam("usedNonce", "uint256", False),
      ],
    )

  def test_extract_generated_events_maps_tuple_records(self) -> None:
    events = extract_generated_events(
      """
private def morphoEvents : List EventDef := [
  { name := "CreateMarket", params := [⟨"id", .bytes32, .indexed⟩, ⟨"marketParams", .tuple [.address, .address, .address, .address, .uint256], .unindexed⟩] },
  { name := "IncrementNonce", params := [⟨"caller", .address, .indexed⟩, ⟨"authorizer", .address, .indexed⟩, ⟨"usedNonce", .uint256, .unindexed⟩] }
]
"""
    )
    self.assertEqual(
      events["CreateMarket"],
      [
        EventParam("id", "bytes32", True),
        EventParam("marketParams", "tuple(address,address,address,address,uint256)", False),
      ],
    )

  def test_validate_event_surface_reports_missing_event(self) -> None:
    solidity = {"IncrementNonce": [EventParam("usedNonce", "uint256", False)]}
    generated: dict[str, list[EventParam]] = {}
    with self.assertRaisesRegex(MorphoEventSurfaceError, "missing generated events: IncrementNonce"):
      validate_event_surface(solidity, generated)

  def test_validate_event_surface_reports_payload_mismatch(self) -> None:
    solidity = {
      "CreateMarket": [
        EventParam("id", "bytes32", True),
        EventParam("marketParams", "tuple(address,address,address,address,uint256)", False),
      ]
    }
    generated = {"CreateMarket": [EventParam("id", "bytes32", True)]}
    with self.assertRaisesRegex(MorphoEventSurfaceError, "CreateMarket"):
      validate_event_surface(solidity, generated)

  def test_current_repo_event_surface_matches(self) -> None:
    solidity = extract_solidity_events((ROOT / "morpho-blue" / "src" / "libraries" / "EventsLib.sol").read_text())
    generated = extract_generated_events((ROOT / "Morpho" / "Compiler" / "Generated.lean").read_text())
    validate_event_surface(solidity, generated)

  def test_cli_reports_checker_error_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmpdir:
      root = pathlib.Path(tmpdir)
      events_path = root / "EventsLib.sol"
      generated_path = root / "Generated.lean"
      events_path.write_text("library EventsLib { event IncrementNonce(uint256 usedNonce); }\n", encoding="utf-8")
      generated_path.write_text("private def morphoEvents : List EventDef := []\n", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(ROOT / "scripts" / "check_morpho_event_surface.py"),
          "--events-lib",
          str(events_path),
          "--generated",
          str(generated_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

    self.assertNotEqual(proc.returncode, 0)
    self.assertIn("morpho-event-surface check failed:", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)


if __name__ == "__main__":
  unittest.main()
