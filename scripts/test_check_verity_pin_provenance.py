#!/usr/bin/env python3
"""Unit tests for Verity pin provenance checking."""

from __future__ import annotations

import pathlib
import sys
import tempfile
import textwrap
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

from check_verity_pin_provenance import main as check_main  # noqa: E402


class CheckVerityPinProvenanceTests(unittest.TestCase):
  def run_check(
    self,
    *,
    lakefile_text: str,
    manifest_text: str,
    provenance_text: str,
    doc_text: str,
    obligations_text: str = """
    {
      "issueClusters": [
        {
          "issue": 123,
          "title": "Implement supply/withdraw/borrow/repay in the Verity EDSL"
        },
        {
          "issue": 124,
          "title": "Implement collateral and liquidation flows in the Verity EDSL"
        }
      ]
    }
    """,
    readme_text: str = "See docs/VERITY_PIN.md for the pinned Verity revision.\n",
  ) -> int:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      (root / "config").mkdir()
      (root / "docs").mkdir()
      (root / "Morpho/Compiler").mkdir(parents=True)
      (root / "Morpho/Proofs").mkdir(parents=True)
      (root / "scripts").mkdir()
      (root / "lakefile.lean").write_text(textwrap.dedent(lakefile_text), encoding="utf-8")
      (root / "lake-manifest.json").write_text(textwrap.dedent(manifest_text), encoding="utf-8")
      (root / "config/verity-pin-provenance.json").write_text(
        textwrap.dedent(provenance_text),
        encoding="utf-8",
      )
      (root / "config/semantic-bridge-obligations.json").write_text(
        textwrap.dedent(obligations_text),
        encoding="utf-8",
      )
      (root / "docs/VERITY_PIN.md").write_text(textwrap.dedent(doc_text), encoding="utf-8")
      (root / "README.md").write_text(readme_text, encoding="utf-8")
      for rel in (
        "Morpho/Compiler/MacroSlice.lean",
        "Morpho/Compiler/Generated.lean",
        "Morpho/Compiler/AdminAdapters.lean",
        "Morpho/Proofs/SemanticBridgeDischarge.lean",
        "Morpho/Proofs/SemanticBridgeInstantiation.lean",
        "Morpho/Proofs/SemanticBridgeReadiness.lean",
        "scripts/check_macro_migration_blockers.py",
      ):
        (root / rel).write_text("-- placeholder\n", encoding="utf-8")
      argv = sys.argv[:]
      try:
        sys.argv = [
          "check_verity_pin_provenance.py",
          "--lakefile",
          str(root / "lakefile.lean"),
          "--manifest",
          str(root / "lake-manifest.json"),
          "--provenance",
          str(root / "config/verity-pin-provenance.json"),
          "--doc",
          str(root / "docs/VERITY_PIN.md"),
          "--readme",
          str(root / "README.md"),
          "--obligations",
          str(root / "config/semantic-bridge-obligations.json"),
        ]
        return check_main()
      finally:
        sys.argv = argv

  def test_accepts_synced_provenance(self) -> None:
    rc = self.run_check(
      lakefile_text="""
      require verity from git
        "https://github.com/Th0rgal/verity.git" @ "9d9533b2"
      """,
      manifest_text="""
      {
        "packages": [
          {
            "name": "verity",
            "url": "https://github.com/Th0rgal/verity.git",
            "rev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
            "inputRev": "9d9533b2"
          }
        ]
      }
      """,
      provenance_text="""
      {
        "upstreamRepo": "https://github.com/Th0rgal/verity.git",
        "inputRev": "9d9533b2",
        "fullRev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
        "trackedIssue": "#118",
        "whyPinned": "Current deterministic base.",
        "remainingDivergences": [
          {
            "area": "Local generated-contract boundary",
            "summary": "Repo-local generated boundary remains.",
            "files": [
              "Morpho/Compiler/MacroSlice.lean",
              "Morpho/Compiler/Generated.lean"
            ]
          },
          {
            "area": "Upstream macro/frontend gaps still block operation migration",
            "summary": "Several operations remain blocked at the current pin on internal calls, ERC20 helpers, callbacks, 2D struct access, direct mstore/mload, pure-expression externalCall, usable blockTimestamp values, and dynamic-topic rawLog witnesses.",
            "issueClusters": [
              "#123",
              "#124"
            ],
            "blockers": [
              "internal calls",
              "ERC20 helpers",
              "callbacks",
              "2D struct access",
              "direct mstore/mload",
              "pure-expression externalCall",
              "usable blockTimestamp values",
              "dynamic-topic rawLog witnesses"
            ],
            "files": [
              "Morpho/Compiler/MacroSlice.lean",
              "Morpho/Proofs/SemanticBridgeReadiness.lean",
              "scripts/check_macro_migration_blockers.py"
            ]
          }
        ]
      }
      """,
      doc_text="""
      https://github.com/Th0rgal/verity.git
      9d9533b2
      9d9533b2e8fd775ed673797b6a95301c8414c675
      #118
        Current deterministic base.
        Local generated-contract boundary
        Repo-local generated boundary remains.
        Morpho/Compiler/MacroSlice.lean
        Morpho/Compiler/Generated.lean
        Upstream macro/frontend gaps still block operation migration
        Several operations remain blocked at the current pin on internal calls, ERC20 helpers, callbacks, 2D struct access, direct mstore/mload, pure-expression externalCall, usable blockTimestamp values, and dynamic-topic rawLog witnesses.
        internal calls
        ERC20 helpers
        callbacks
        2D struct access
        direct mstore/mload
        pure-expression externalCall
        usable blockTimestamp values
        dynamic-topic rawLog witnesses
        #123
        #124
        Morpho/Proofs/SemanticBridgeReadiness.lean
        scripts/check_macro_migration_blockers.py
        """,
      )
    self.assertEqual(rc, 0)

  def test_rejects_missing_macro_frontend_blockers(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "9d9533b2"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
              "inputRev": "9d9533b2"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "9d9533b2",
          "fullRev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
          "trackedIssue": "#118",
          "whyPinned": "Current deterministic base.",
          "remainingDivergences": [
            {
              "area": "Upstream macro/frontend gaps still block operation migration",
              "summary": "Still blocked.",
              "issueClusters": [
                "#123"
              ],
              "files": [
                "Morpho/Compiler/MacroSlice.lean"
              ]
            }
          ]
        }
        """,
        doc_text="""
        https://github.com/Th0rgal/verity.git
        9d9533b2
        9d9533b2e8fd775ed673797b6a95301c8414c675
        #118
        Current deterministic base.
        Upstream macro/frontend gaps still block operation migration
        Still blocked.
        Morpho/Compiler/MacroSlice.lean
        """,
      )

  def test_rejects_missing_macro_frontend_issue_clusters(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "9d9533b2"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
              "inputRev": "9d9533b2"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "9d9533b2",
          "fullRev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
          "trackedIssue": "#118",
          "whyPinned": "Current deterministic base.",
          "remainingDivergences": [
            {
              "area": "Upstream macro/frontend gaps still block operation migration",
              "summary": "Still blocked.",
              "blockers": [
                "internal calls"
              ],
              "files": [
                "Morpho/Compiler/MacroSlice.lean"
              ]
            }
          ]
        }
        """,
        doc_text="""
        https://github.com/Th0rgal/verity.git
        9d9533b2
        9d9533b2e8fd775ed673797b6a95301c8414c675
        #118
        Current deterministic base.
        Upstream macro/frontend gaps still block operation migration
        Still blocked.
        internal calls
        Morpho/Compiler/MacroSlice.lean
        """,
      )

  def test_rejects_unknown_macro_frontend_issue_cluster(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "9d9533b2"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
              "inputRev": "9d9533b2"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "9d9533b2",
          "fullRev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
          "trackedIssue": "#118",
          "whyPinned": "Current deterministic base.",
          "remainingDivergences": [
            {
              "area": "Upstream macro/frontend gaps still block operation migration",
              "summary": "Still blocked.",
              "issueClusters": [
                "#999"
              ],
              "blockers": [
                "internal calls"
              ],
              "files": [
                "Morpho/Compiler/MacroSlice.lean"
              ]
            }
          ]
        }
        """,
        doc_text="""
        https://github.com/Th0rgal/verity.git
        9d9533b2
        9d9533b2e8fd775ed673797b6a95301c8414c675
        #118
        Current deterministic base.
        Upstream macro/frontend gaps still block operation migration
        Still blocked.
        #999
        internal calls
        Morpho/Compiler/MacroSlice.lean
        """,
      )

  def test_rejects_full_rev_mismatch(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "9d9533b2"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
              "inputRev": "9d9533b2"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "9d9533b2",
          "fullRev": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
          "trackedIssue": "#118",
          "whyPinned": "Current deterministic base.",
          "remainingDivergences": [
            {
              "area": "Local generated-contract boundary",
              "summary": "Repo-local generated boundary remains.",
              "files": [
                "Morpho/Compiler/MacroSlice.lean"
              ]
            }
          ]
        }
        """,
        doc_text="""
        https://github.com/Th0rgal/verity.git
        9d9533b2
        aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
        #118
        Current deterministic base.
        Local generated-contract boundary
        Repo-local generated boundary remains.
        Morpho/Compiler/MacroSlice.lean
        """,
      )

  def test_rejects_missing_readme_reference(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "9d9533b2"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
              "inputRev": "9d9533b2"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "9d9533b2",
          "fullRev": "9d9533b2e8fd775ed673797b6a95301c8414c675",
          "trackedIssue": "#118",
          "whyPinned": "Current deterministic base.",
          "remainingDivergences": [
            {
              "area": "Local generated-contract boundary",
              "summary": "Repo-local generated boundary remains.",
              "files": [
                "Morpho/Compiler/MacroSlice.lean"
              ]
            }
          ]
        }
        """,
        doc_text="""
        https://github.com/Th0rgal/verity.git
        9d9533b2
        9d9533b2e8fd775ed673797b6a95301c8414c675
        #118
        Current deterministic base.
        Local generated-contract boundary
        Repo-local generated boundary remains.
        Morpho/Compiler/MacroSlice.lean
        """,
        readme_text="No pin doc linked here.\n",
      )
