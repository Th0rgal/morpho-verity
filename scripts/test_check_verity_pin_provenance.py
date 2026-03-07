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


def make_macro_frontend_doc(
  *,
  summary: str,
  blockers: list[str],
  issue_clusters: list[str],
  files: list[str],
  upstream_repo: str = "https://github.com/Th0rgal/verity.git",
  input_rev: str = "9d9533b2",
  full_rev: str = "9d9533b2e8fd775ed673797b6a95301c8414c675",
  tracked_issue: str = "#118",
  why_pinned: str = "Current deterministic base.",
) -> str:
  blocker_lines = "\n".join(f"- {item}" for item in blockers)
  cluster_lines = "\n".join(f"- {item}" for item in issue_clusters)
  file_lines = "\n".join(f"- `{item}`" for item in files)
  return textwrap.dedent(
    f"""
    # Verity Pin

    - Repo: `{upstream_repo}`
    - Short rev: `{input_rev}`
    - Full rev: `{full_rev}`
    - Tracking issue: `{tracked_issue}`

    ## Why this pin

    {why_pinned}

    ## Remaining repo-local divergence at this pin

    ### Upstream macro/frontend gaps still block operation migration

    {summary}

    Current blocker families at this pin:
    {blocker_lines}

    Tracked migration issue clusters:
    {cluster_lines}

    Relevant files:
    {file_lines}

    ## Enforcement

    `config/verity-pin-provenance.json`
    """
  ).strip()


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
            "area": "Repo-local state encoding wrappers",
            "summary": "Semantic-bridge proofs still rely on repo-local MorphoState-to-ContractState encoders and wrapper theorems around the upstream contract semantics surface.",
            "files": [
              "Morpho/Compiler/AdminAdapters.lean",
              "Morpho/Proofs/SemanticBridgeDischarge.lean",
              "Morpho/Proofs/SemanticBridgeInstantiation.lean"
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
      # Verity Pin

      - Repo: `https://github.com/Th0rgal/verity.git`
      - Short rev: `9d9533b2`
      - Full rev: `9d9533b2e8fd775ed673797b6a95301c8414c675`
      - Tracking issue: `#118`

      ## Why this pin

      Current deterministic base.

      ## Remaining repo-local divergence at this pin

      ### Local generated-contract boundary

      Repo-local generated boundary remains.

      Relevant files:
      - `Morpho/Compiler/MacroSlice.lean`
      - `Morpho/Compiler/Generated.lean`

      ### Repo-local state encoding wrappers

      Semantic-bridge proofs still rely on repo-local MorphoState-to-ContractState encoders and wrapper theorems around the upstream contract semantics surface.

      Relevant files:
      - `Morpho/Compiler/AdminAdapters.lean`
      - `Morpho/Proofs/SemanticBridgeDischarge.lean`
      - `Morpho/Proofs/SemanticBridgeInstantiation.lean`

      ### Upstream macro/frontend gaps still block operation migration

      Several operations remain blocked at the current pin on internal calls, ERC20 helpers, callbacks, 2D struct access, direct mstore/mload, pure-expression externalCall, usable blockTimestamp values, and dynamic-topic rawLog witnesses.

      Current blocker families at this pin:
      - internal calls
      - ERC20 helpers
      - callbacks
      - 2D struct access
      - direct `mstore`/`mload`
      - pure-expression externalCall
      - usable blockTimestamp values
      - dynamic-topic rawLog witnesses

      Tracked migration issue clusters:
      - #123
      - #124

      Relevant files:
      - `Morpho/Compiler/MacroSlice.lean`
      - `Morpho/Proofs/SemanticBridgeReadiness.lean`
      - `scripts/check_macro_migration_blockers.py`

      ## Enforcement

      `config/verity-pin-provenance.json`
      """,
      )
    self.assertEqual(rc, 0)

  def test_rejects_stale_verity_pin_metadata_bullet(self) -> None:
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
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.",
          blockers=["internal calls"],
          issue_clusters=["#123"],
          files=["Morpho/Compiler/MacroSlice.lean"],
          input_rev="aaaaaaaa",
        ),
      )

  def test_rejects_duplicate_verity_pin_metadata_bullet(self) -> None:
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
        doc_text=textwrap.dedent(
          """
          # Verity Pin

          - Repo: `https://github.com/Th0rgal/verity.git`
          - Short rev: `9d9533b2`
          - Full rev: `9d9533b2e8fd775ed673797b6a95301c8414c675`
          - Full rev: `9d9533b2e8fd775ed673797b6a95301c8414c675`
          - Tracking issue: `#118`

          ## Why this pin

          Current deterministic base.

          ## Remaining repo-local divergence at this pin

          ### Upstream macro/frontend gaps still block operation migration

          Still blocked.

          Current blocker families at this pin:
          - internal calls

          Tracked migration issue clusters:
          - #123

          Relevant files:
          - `Morpho/Compiler/MacroSlice.lean`

          ## Enforcement

          `config/verity-pin-provenance.json`
          """
        ),
      )

  def test_rejects_stale_preamble_metadata_before_bullets(self) -> None:
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
        doc_text=textwrap.dedent(
          """
          # Verity Pin

          `morpho-verity` currently pins Verity to:
          Repo: `https://github.com/Th0rgal/legacy-verity.git`

          - Repo: `https://github.com/Th0rgal/verity.git`
          - Short rev: `9d9533b2`
          - Full rev: `9d9533b2e8fd775ed673797b6a95301c8414c675`
          - Tracking issue: `#118`

          ## Why this pin

          Current deterministic base.

          ## Remaining repo-local divergence at this pin

          ### Upstream macro/frontend gaps still block operation migration

          Still blocked.

          Current blocker families at this pin:
          - internal calls

          Tracked migration issue clusters:
          - #123

          Relevant files:
          - `Morpho/Compiler/MacroSlice.lean`

          ## Enforcement

          `config/verity-pin-provenance.json`
          """
        ),
      )

  def test_rejects_stale_why_pinned_summary(self) -> None:
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
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.",
          blockers=["internal calls"],
          issue_clusters=["#123"],
          files=["Morpho/Compiler/MacroSlice.lean"],
          why_pinned="Current deterministic base.\n\nExtra stale rationale.",
        ),
      )

  def test_accepts_divergence_summary_line_ending_with_colon(self) -> None:
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
            "area": "Upstream macro/frontend gaps still block operation migration",
            "summary": "Still blocked:",
            "issueClusters": [
              "#123"
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
      doc_text=make_macro_frontend_doc(
        summary="Still blocked:",
        blockers=["internal calls"],
        issue_clusters=["#123"],
        files=["Morpho/Compiler/MacroSlice.lean"],
      ),
    )
    self.assertEqual(rc, 0)

  def test_rejects_stale_divergence_summary(self) -> None:
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
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.\n\nStale extra explanation.",
          blockers=["internal calls"],
          issue_clusters=["#123"],
          files=["Morpho/Compiler/MacroSlice.lean"],
        ),
      )

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
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.",
          blockers=["internal calls"],
          issue_clusters=["#123"],
          files=["Morpho/Compiler/MacroSlice.lean"],
        ),
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
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.",
          blockers=["internal calls"],
          issue_clusters=["#123"],
          files=["Morpho/Compiler/MacroSlice.lean"],
        ),
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
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.",
          blockers=["internal calls"],
          issue_clusters=["#999"],
          files=["Morpho/Compiler/MacroSlice.lean"],
        ),
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
        # Verity Pin

        - Repo: `https://github.com/Th0rgal/verity.git`
        - Short rev: `9d9533b2`
        - Full rev: `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Local generated-contract boundary

        Repo-local generated boundary remains.

        Relevant files:
        - `Morpho/Compiler/MacroSlice.lean`
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
        # Verity Pin

        - Repo: `https://github.com/Th0rgal/verity.git`
        - Short rev: `9d9533b2`
        - Full rev: `9d9533b2e8fd775ed673797b6a95301c8414c675`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Local generated-contract boundary

        Repo-local generated boundary remains.

        Relevant files:
        - `Morpho/Compiler/MacroSlice.lean`
        """,
        readme_text="No pin doc linked here.\n",
      )

  def test_rejects_stale_macro_frontend_blocker_bullet(self) -> None:
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
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.",
          blockers=["internal calls", "ERC20 helpers"],
          issue_clusters=["#123"],
          files=["Morpho/Compiler/MacroSlice.lean"],
        ),
      )

  def test_rejects_duplicate_macro_frontend_issue_cluster_bullet(self) -> None:
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
                "#123",
                "#124"
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
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.",
          blockers=["internal calls"],
          issue_clusters=["#123", "#123", "#124"],
          files=["Morpho/Compiler/MacroSlice.lean"],
        ),
      )

  def test_rejects_stale_non_macro_divergence_file_bullet(self) -> None:
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
        # Verity Pin

        - Repo: `https://github.com/Th0rgal/verity.git`
        - Short rev: `9d9533b2`
        - Full rev: `9d9533b2e8fd775ed673797b6a95301c8414c675`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Local generated-contract boundary

        Repo-local generated boundary remains.

        Relevant files:
        - `Morpho/Compiler/MacroSlice.lean`
        - `Morpho/Compiler/Generated.lean`
        """,
      )

  def test_rejects_duplicate_non_macro_divergence_file_bullet(self) -> None:
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
              "area": "Repo-local state encoding wrappers",
              "summary": "Repo-local wrappers remain.",
              "files": [
                "Morpho/Compiler/AdminAdapters.lean",
                "Morpho/Proofs/SemanticBridgeDischarge.lean"
              ]
            }
          ]
        }
        """,
        doc_text="""
        # Verity Pin

        - Repo: `https://github.com/Th0rgal/verity.git`
        - Short rev: `9d9533b2`
        - Full rev: `9d9533b2e8fd775ed673797b6a95301c8414c675`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Repo-local state encoding wrappers

        Repo-local wrappers remain.

        Relevant files:
        - `Morpho/Compiler/AdminAdapters.lean`
        - `Morpho/Compiler/AdminAdapters.lean`
        - `Morpho/Proofs/SemanticBridgeDischarge.lean`
        """,
      )
