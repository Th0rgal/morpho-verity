#!/usr/bin/env python3
"""Unit tests for Verity pin provenance checking."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import textwrap
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

from check_verity_pin_provenance import (  # noqa: E402
  EXPECTED_ENFORCEMENT_TEXT,
  EXPECTED_WORKFLOW_RUN_LINES,
  EXPECTED_WORKFLOW_STEPS,
  main as check_main,
  validate_divergence_section_headings,
  validate_enforcement_section,
  validate_workflow,
)


def make_macro_frontend_doc(
  *,
  summary: str,
  blockers: list[str],
  issue_clusters: list[str],
  files: list[str],
  upstream_repo: str = "https://github.com/Th0rgal/verity.git",
  input_rev: str = "ad03fc64",
  full_rev: str = "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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

    The machine-readable source of truth is
    `config/verity-pin-provenance.json`. CI checks that it stays in sync with
    `lakefile.lean` and `lake-manifest.json` via
    `scripts/check_verity_pin_provenance.py`.
    """
  ).strip()


def make_verify_workflow(*, steps: list[str] | None = None) -> str:
  steps = list(EXPECTED_WORKFLOW_STEPS) if steps is None else steps
  return "\n".join([
    "jobs:",
    "  verify:",
    "    steps:",
    *[
      "\n".join([
        f"      - name: {step}",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES.get(step, 'echo placeholder')}",
      ])
      for step in steps
    ],
    "",
  ])


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
    workflow_text: str | None = None,
  ) -> int:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      (root / "config").mkdir()
      (root / "docs").mkdir()
      (root / ".github/workflows").mkdir(parents=True)
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
      (root / ".github/workflows/verify.yml").write_text(
        make_verify_workflow() if workflow_text is None else textwrap.dedent(workflow_text),
        encoding="utf-8",
      )
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
          "--workflow",
          str(root / ".github/workflows/verify.yml"),
        ]
        return check_main()
      finally:
        sys.argv = argv

  def run_cli(self, root: pathlib.Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
      [sys.executable, str(pathlib.Path(__file__).resolve().parent / "check_verity_pin_provenance.py"), *args],
      cwd=root,
      text=True,
      capture_output=True,
      check=False,
    )

  def test_validate_enforcement_section_passes(self) -> None:
    validate_enforcement_section(
      doc_text="\n".join([
        "# Verity Pin",
        "",
        "## Enforcement",
        "",
        "The machine-readable source of truth is",
        "`config/verity-pin-provenance.json`. CI checks that it stays in sync with",
        "`lakefile.lean` and `lake-manifest.json` via",
        "`scripts/check_verity_pin_provenance.py`.",
      ]),
      doc_path=pathlib.Path("docs/VERITY_PIN.md"),
    )

  def test_validate_enforcement_section_rejects_stale_text(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      validate_enforcement_section(
        doc_text="\n".join([
          "# Verity Pin",
          "",
          "## Enforcement",
          "",
          EXPECTED_ENFORCEMENT_TEXT,
          "",
          "Extra stale note.",
        ]),
        doc_path=pathlib.Path("docs/VERITY_PIN.md"),
      )

  def test_validate_enforcement_section_accepts_crlf_heading_lines(self) -> None:
    validate_enforcement_section(
      doc_text="\r\n".join([
        "# Verity Pin",
        "",
        "## Enforcement",
        "",
        "The machine-readable source of truth is",
        "`config/verity-pin-provenance.json`. CI checks that it stays in sync with",
        "`lakefile.lean` and `lake-manifest.json` via",
        "`scripts/check_verity_pin_provenance.py`.",
      ]),
      doc_path=pathlib.Path("docs/VERITY_PIN.md"),
    )

  def test_validate_enforcement_section_ignores_later_h2_appendix(self) -> None:
    validate_enforcement_section(
      doc_text="\n".join([
        "# Verity Pin",
        "",
        "## Enforcement",
        "",
        "The machine-readable source of truth is",
        "`config/verity-pin-provenance.json`. CI checks that it stays in sync with",
        "`lakefile.lean` and `lake-manifest.json` via",
        "`scripts/check_verity_pin_provenance.py`.",
        "",
        "## Appendix",
        "",
        "Supplemental notes.",
      ]),
      doc_path=pathlib.Path("docs/VERITY_PIN.md"),
    )

  def test_validate_divergence_section_headings_passes(self) -> None:
    validate_divergence_section_headings(
      doc_text="\n".join([
        "# Verity Pin",
        "",
        "## Remaining repo-local divergence at this pin",
        "",
        "### Local generated-contract boundary",
        "",
        "Summary.",
        "",
        "### Repo-local state encoding wrappers",
        "",
        "Summary.",
        "",
        "## Enforcement",
      ]),
      divergences=[
        {"area": "Local generated-contract boundary"},
        {"area": "Repo-local state encoding wrappers"},
      ],
      doc_path=pathlib.Path("docs/VERITY_PIN.md"),
    )

  def test_validate_divergence_section_headings_rejects_extra_heading(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      validate_divergence_section_headings(
        doc_text="\n".join([
          "# Verity Pin",
          "",
          "## Remaining repo-local divergence at this pin",
          "",
          "### Local generated-contract boundary",
          "",
          "Summary.",
          "",
          "### Stale extra divergence",
          "",
          "Summary.",
          "",
          "## Enforcement",
        ]),
        divergences=[
          {"area": "Local generated-contract boundary"},
        ],
        doc_path=pathlib.Path("docs/VERITY_PIN.md"),
      )

  def test_validate_workflow_passes(self) -> None:
    validate_workflow(
      workflow_text=make_verify_workflow(),
      workflow_path=pathlib.Path(".github/workflows/verify.yml"),
    )

  def test_validate_workflow_accepts_trailing_comments_on_step_names(self) -> None:
    validate_workflow(
      workflow_text="\n".join([
        "jobs:",
        "  verify:",
        "    steps:",
        *[
          "\n".join([
            f"      - name: {step} # tracked workflow step",
            f"        run: {EXPECTED_WORKFLOW_RUN_LINES[step]}",
          ])
          for step in EXPECTED_WORKFLOW_STEPS
        ],
        "",
      ]),
      workflow_path=pathlib.Path(".github/workflows/verify.yml"),
    )

  def test_validate_workflow_accepts_tagged_step_names(self) -> None:
    validate_workflow(
      workflow_text="\n".join([
        "jobs:",
        "  verify:",
        "    steps:",
        *[
          "\n".join([
            f'      - name: !!str "{step}"',
            f"        run: {EXPECTED_WORKFLOW_RUN_LINES[step]}",
          ])
          for step in EXPECTED_WORKFLOW_STEPS
        ],
        "",
      ]),
      workflow_path=pathlib.Path(".github/workflows/verify.yml"),
    )

  def test_validate_workflow_accepts_tagged_and_anchored_run_scalars(self) -> None:
    validate_workflow(
      workflow_text="\n".join([
        "jobs:",
        "  verify:",
        "    steps:",
        f"      - name: {EXPECTED_WORKFLOW_STEPS[0]}",
        f'        run: !!str {EXPECTED_WORKFLOW_RUN_LINES[EXPECTED_WORKFLOW_STEPS[0]]}',
        f"      - name: {EXPECTED_WORKFLOW_STEPS[1]}",
        f"        run: &shared_run {EXPECTED_WORKFLOW_RUN_LINES[EXPECTED_WORKFLOW_STEPS[1]]}",
        "",
      ]),
      workflow_path=pathlib.Path(".github/workflows/verify.yml"),
    )

  def test_validate_workflow_rejects_missing_step(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      validate_workflow(
        workflow_text=make_verify_workflow(steps=list(EXPECTED_WORKFLOW_STEPS[:-1])),
        workflow_path=pathlib.Path(".github/workflows/verify.yml"),
      )

  def test_validate_workflow_rejects_commented_step_name(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      validate_workflow(
        workflow_text="\n".join([
          "jobs:",
          "  verify:",
          "    steps:",
          "      # - name: Validate verity pin sync",
          "      - name: Validate verity pin provenance",
          "",
        ]),
        workflow_path=pathlib.Path(".github/workflows/verify.yml"),
      )

  def test_validate_workflow_rejects_duplicate_step(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      validate_workflow(
        workflow_text=make_verify_workflow(
          steps=[
            EXPECTED_WORKFLOW_STEPS[0],
            EXPECTED_WORKFLOW_STEPS[0],
            EXPECTED_WORKFLOW_STEPS[1],
          ]
        ),
        workflow_path=pathlib.Path(".github/workflows/verify.yml"),
      )

  def test_validate_workflow_rejects_wrong_run_command(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      validate_workflow(
        workflow_text="\n".join([
          "jobs:",
          "  verify:",
          "    steps:",
          "      - name: Validate verity pin sync",
          "        run: echo noop",
          "      - name: Validate verity pin provenance",
          f"        run: {EXPECTED_WORKFLOW_RUN_LINES['Validate verity pin provenance']}",
          "",
        ]),
        workflow_path=pathlib.Path(".github/workflows/verify.yml"),
      )

  def test_validate_workflow_ignores_unnamed_run_step_after_tracked_step(self) -> None:
    validate_workflow(
      workflow_text="\n".join([
        "jobs:",
        "  verify:",
        "    steps:",
        "      - name: Validate verity pin sync",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES['Validate verity pin sync']}",
        "      - run: echo helper step",
        "      - name: Validate verity pin provenance",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES['Validate verity pin provenance']}",
        "",
      ]),
      workflow_path=pathlib.Path(".github/workflows/verify.yml"),
    )

  def test_validate_workflow_ignores_multiline_run_on_unnamed_step(self) -> None:
    validate_workflow(
      workflow_text="\n".join([
        "jobs:",
        "  verify:",
        "    steps:",
        "      - name: Validate verity pin sync",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES['Validate verity pin sync']}",
        "      - env:",
        "          HELPER: 1",
        "        run: echo helper step",
        "      - name: Validate verity pin provenance",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES['Validate verity pin provenance']}",
        "",
      ]),
      workflow_path=pathlib.Path(".github/workflows/verify.yml"),
    )

  def test_validate_workflow_ignores_nested_run_mapping_under_tracked_step(self) -> None:
    validate_workflow(
      workflow_text="\n".join([
        "jobs:",
        "  verify:",
        "    steps:",
        "      - name: Validate verity pin sync",
        "        with:",
        "          run: echo helper metadata",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES['Validate verity pin sync']}",
        "      - name: Validate verity pin provenance",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES['Validate verity pin provenance']}",
        "",
      ]),
      workflow_path=pathlib.Path(".github/workflows/verify.yml"),
    )

  def test_accepts_synced_provenance(self) -> None:
    rc = self.run_check(
      lakefile_text="""
      require verity from git
        "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
      """,
      manifest_text="""
      {
        "packages": [
          {
            "name": "verity",
            "url": "https://github.com/Th0rgal/verity.git",
            "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
            "inputRev": "ad03fc64"
          }
        ]
      }
      """,
      provenance_text="""
      {
        "upstreamRepo": "https://github.com/Th0rgal/verity.git",
        "inputRev": "ad03fc64",
        "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
      - Short rev: `ad03fc64`
      - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
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

      The machine-readable source of truth is
      `config/verity-pin-provenance.json`. CI checks that it stays in sync with
      `lakefile.lean` and `lake-manifest.json` via
      `scripts/check_verity_pin_provenance.py`.
      """,
      )
    self.assertEqual(rc, 0)

  def test_rejects_invalid_provenance_json(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text='{"upstreamRepo": ',
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.",
          blockers=["internal calls"],
          issue_clusters=["#123"],
          files=["Morpho/Compiler/MacroSlice.lean"],
        ),
      )

  def test_rejects_non_object_issue_cluster_root(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        obligations_text="""
        [
          {
            "issue": 123,
            "title": "Implement supply/withdraw/borrow/repay in the Verity EDSL"
          }
        ]
        """,
        doc_text=make_macro_frontend_doc(
          summary="Still blocked.",
          blockers=["internal calls"],
          issue_clusters=["#123"],
          files=["Morpho/Compiler/MacroSlice.lean"],
        ),
      )

  def test_rejects_duplicate_issue_cluster_numbers(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        obligations_text="""
        {
          "issueClusters": [
            {
              "issue": 123,
              "title": "Implement supply/withdraw/borrow/repay in the Verity EDSL"
            },
            {
              "issue": 123,
              "title": "Conflicting duplicate issue cluster"
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

  def test_rejects_stale_verity_pin_metadata_bullet(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          - Short rev: `ad03fc64`
          - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
          - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          - Short rev: `ad03fc64`
          - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
      """,
      manifest_text="""
      {
        "packages": [
          {
            "name": "verity",
            "url": "https://github.com/Th0rgal/verity.git",
            "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
            "inputRev": "ad03fc64"
          }
        ]
      }
      """,
      provenance_text="""
      {
        "upstreamRepo": "https://github.com/Th0rgal/verity.git",
        "inputRev": "ad03fc64",
        "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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

  def test_rejects_stale_enforcement_summary(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        ).replace(
          "`scripts/check_verity_pin_provenance.py`.",
          "`scripts/check_verity_pin_provenance.py`.\n\nExtra stale note.",
        ),
      )

  def test_rejects_missing_workflow_enforcement_step(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        ),
        workflow_text=make_verify_workflow(steps=list(EXPECTED_WORKFLOW_STEPS[:-1])),
      )

  def test_rejects_commented_workflow_enforcement_step(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        ),
        workflow_text="""
        jobs:
          verify:
            steps:
              # - name: Validate verity pin sync
              - name: Validate verity pin provenance
        """,
      )

  def test_rejects_duplicate_workflow_enforcement_step(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        ),
        workflow_text=make_verify_workflow(
          steps=[
            EXPECTED_WORKFLOW_STEPS[0],
            EXPECTED_WORKFLOW_STEPS[0],
            EXPECTED_WORKFLOW_STEPS[1],
          ]
        ),
      )

  def test_rejects_wrong_workflow_enforcement_command(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        ),
        workflow_text="\n".join([
          "jobs:",
          "  verify:",
          "    steps:",
          "      - name: Validate verity pin sync",
          "        run: echo noop",
          "      - name: Validate verity pin provenance",
          f"        run: {EXPECTED_WORKFLOW_RUN_LINES['Validate verity pin provenance']}",
          "",
        ]),
      )

  def test_rejects_full_rev_mismatch(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
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
        - Short rev: `ad03fc64`
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
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

  def test_rejects_boolean_issue_cluster_id_true(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        ),
        obligations_text="""
        {
          "issueClusters": [
            {
              "issue": true,
              "title": "Implement supply/withdraw/borrow/repay in the Verity EDSL"
            }
          ]
        }
        """,
      )

  def test_rejects_boolean_issue_cluster_id_false(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        ),
        obligations_text="""
        {
          "issueClusters": [
            {
              "issue": false,
              "title": "Implement supply/withdraw/borrow/repay in the Verity EDSL"
            }
          ]
        }
        """,
      )

  def test_rejects_stale_macro_frontend_blocker_bullet(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
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
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
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

  def test_rejects_stale_extra_divergence_section(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Local generated-contract boundary

        Repo-local generated boundary remains.

        Relevant files:
        - `Morpho/Compiler/MacroSlice.lean`

        ### Stale extra divergence

        This stale section is not tracked in JSON.

        Relevant files:
        - `Morpho/Compiler/Generated.lean`
        """,
      )

  def test_rejects_prefixed_fake_why_section_masking_real_drift(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
          "trackedIssue": "#118",
          "whyPinned": "Current deterministic base.",
          "remainingDivergences": [
            {
              "area": "Upstream macro/frontend gaps still block operation migration",
              "summary": "Current deterministic base.",
              "blockers": ["internal calls"],
              "issueClusters": ["#123"],
              "files": ["scripts/check_macro_migration_blockers.py"]
            }
          ]
        }
        """,
        doc_text="""
        ## Why this pin

        Current deterministic base.

        # Verity Pin

        - Repo: `https://github.com/Th0rgal/verity.git`
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
        - Tracking issue: `#118`

        ## Why this pin

        Drifted why-pinned text.

        ## Remaining repo-local divergence at this pin

        ### Upstream macro/frontend gaps still block operation migration

        Current deterministic base.

        Current blocker families at this pin:
        - internal calls

        Tracked migration issue clusters:
        - `#123`

        Relevant files:
        - `scripts/check_macro_migration_blockers.py`

        ## Enforcement

        The machine-readable source of truth is
        `config/verity-pin-provenance.json`. CI checks that it stays in sync with
        `lakefile.lean` and `lake-manifest.json` via
        `scripts/check_verity_pin_provenance.py`.
        """,
      )

  def test_rejects_prefixed_fake_enforcement_section_masking_real_drift(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
          "trackedIssue": "#118",
          "whyPinned": "Current deterministic base.",
          "remainingDivergences": [
            {
              "area": "Upstream macro/frontend gaps still block operation migration",
              "summary": "Current deterministic base.",
              "blockers": ["internal calls"],
              "issueClusters": ["#123"],
              "files": ["scripts/check_macro_migration_blockers.py"]
            }
          ]
        }
        """,
        doc_text="""
        ## Enforcement

        The machine-readable source of truth is
        `config/verity-pin-provenance.json`. CI checks that it stays in sync with
        `lakefile.lean` and `lake-manifest.json` via
        `scripts/check_verity_pin_provenance.py`.

        # Verity Pin

        - Repo: `https://github.com/Th0rgal/verity.git`
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Upstream macro/frontend gaps still block operation migration

        Current deterministic base.

        Current blocker families at this pin:
        - internal calls

        Tracked migration issue clusters:
        - `#123`

        Relevant files:
        - `scripts/check_macro_migration_blockers.py`

        ## Enforcement

        Drifted enforcement text.
        """,
      )

  def test_rejects_prefixed_fake_divergence_subsection_masking_real_drift(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
          "trackedIssue": "#118",
          "whyPinned": "Current deterministic base.",
          "remainingDivergences": [
            {
              "area": "Upstream macro/frontend gaps still block operation migration",
              "summary": "Current deterministic base.",
              "blockers": ["internal calls"],
              "issueClusters": ["#123"],
              "files": ["scripts/check_macro_migration_blockers.py"]
            }
          ]
        }
        """,
        doc_text="""
        ### Upstream macro/frontend gaps still block operation migration

        Current deterministic base.

        Current blocker families at this pin:
        - internal calls

        Tracked migration issue clusters:
        - `#123`

        Relevant files:
        - `scripts/check_macro_migration_blockers.py`

        # Verity Pin

        - Repo: `https://github.com/Th0rgal/verity.git`
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Upstream macro/frontend gaps still block operation migration

        Drifted macro divergence summary.

        Current blocker families at this pin:
        - internal calls

        Tracked migration issue clusters:
        - `#123`

        Relevant files:
        - `scripts/check_macro_migration_blockers.py`

        ## Enforcement

        The machine-readable source of truth is
        `config/verity-pin-provenance.json`. CI checks that it stays in sync with
        `lakefile.lean` and `lake-manifest.json` via
        `scripts/check_verity_pin_provenance.py`.
        """,
      )

  def test_rejects_duplicate_files_label_masking_real_file_list_drift(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Local generated-contract boundary

        Repo-local generated boundary remains.

        Relevant files:
        - `Morpho/Compiler/MacroSlice.lean`

        Relevant files:
        - `Morpho/Compiler/Generated.lean`

        ## Enforcement

        The machine-readable source of truth is
        `config/verity-pin-provenance.json`. CI checks that it stays in sync with
        `lakefile.lean` and `lake-manifest.json` via
        `scripts/check_verity_pin_provenance.py`.
        """,
      )

  def test_rejects_duplicate_macro_blockers_label_masking_real_blocker_drift(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
          "trackedIssue": "#118",
          "whyPinned": "Current deterministic base.",
          "remainingDivergences": [
            {
              "area": "Upstream macro/frontend gaps still block operation migration",
              "summary": "Current deterministic base.",
              "blockers": ["internal calls"],
              "issueClusters": ["#123"],
              "files": ["scripts/check_macro_migration_blockers.py"]
            }
          ]
        }
        """,
        doc_text="""
        # Verity Pin

        - Repo: `https://github.com/Th0rgal/verity.git`
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Upstream macro/frontend gaps still block operation migration

        Current deterministic base.

        Current blocker families at this pin:
        - internal calls

        Current blocker families at this pin:
        - callbacks

        Tracked migration issue clusters:
        - `#123`

        Relevant files:
        - `scripts/check_macro_migration_blockers.py`

        ## Enforcement

        The machine-readable source of truth is
        `config/verity-pin-provenance.json`. CI checks that it stays in sync with
        `lakefile.lean` and `lake-manifest.json` via
        `scripts/check_verity_pin_provenance.py`.
        """,
      )

  def test_rejects_collapsed_file_list_into_single_bullet(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
            }
          ]
        }
        """,
        doc_text="""
        # Verity Pin

        - Repo: `https://github.com/Th0rgal/verity.git`
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Local generated-contract boundary

        Repo-local generated boundary remains.

        Relevant files:
        - `Morpho/Compiler/MacroSlice.lean` - `Morpho/Compiler/Generated.lean`

        ## Enforcement

        The machine-readable source of truth is
        `config/verity-pin-provenance.json`. CI checks that it stays in sync with
        `lakefile.lean` and `lake-manifest.json` via
        `scripts/check_verity_pin_provenance.py`.
        """,
      )

  def test_rejects_extra_stale_prose_after_expected_divergence_body(self) -> None:
    with self.assertRaisesRegex(SystemExit, "1"):
      self.run_check(
        lakefile_text="""
        require verity from git
          "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
        """,
        manifest_text="""
        {
          "packages": [
            {
              "name": "verity",
              "url": "https://github.com/Th0rgal/verity.git",
              "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
              "inputRev": "ad03fc64"
            }
          ]
        }
        """,
        provenance_text="""
        {
          "upstreamRepo": "https://github.com/Th0rgal/verity.git",
          "inputRev": "ad03fc64",
          "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
        - Short rev: `ad03fc64`
        - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
        - Tracking issue: `#118`

        ## Why this pin

        Current deterministic base.

        ## Remaining repo-local divergence at this pin

        ### Local generated-contract boundary

        Repo-local generated boundary remains.

        Relevant files:
        - `Morpho/Compiler/MacroSlice.lean`

        Stale extra note that is not tracked in the provenance JSON.

        ## Enforcement

        The machine-readable source of truth is
        `config/verity-pin-provenance.json`. CI checks that it stays in sync with
        `lakefile.lean` and `lake-manifest.json` via
        `scripts/check_verity_pin_provenance.py`.
        """,
      )

  def test_cli_reports_invalid_utf8_doc_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      (root / "config").mkdir()
      (root / "docs").mkdir()
      (root / ".github/workflows").mkdir(parents=True)
      (root / "Morpho/Compiler").mkdir(parents=True)
      (root / "Morpho/Proofs").mkdir(parents=True)
      (root / "scripts").mkdir()
      (root / "lakefile.lean").write_text(
        textwrap.dedent(
          """
          require verity from git
            "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
          """
        ),
        encoding="utf-8",
      )
      (root / "lake-manifest.json").write_text(
        textwrap.dedent(
          """
          {
            "packages": [
              {
                "name": "verity",
                "url": "https://github.com/Th0rgal/verity.git",
                "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
                "inputRev": "ad03fc64"
              }
            ]
          }
          """
        ),
        encoding="utf-8",
      )
      (root / "config/verity-pin-provenance.json").write_text(
        textwrap.dedent(
          """
          {
            "upstreamRepo": "https://github.com/Th0rgal/verity.git",
            "inputRev": "ad03fc64",
            "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          """
        ),
        encoding="utf-8",
      )
      (root / "config/semantic-bridge-obligations.json").write_text(
        '{"issueClusters":[{"issue":123,"title":"Track migration gap"}]}',
        encoding="utf-8",
      )
      (root / "docs/VERITY_PIN.md").write_bytes(b"\x80not-utf8")
      (root / "README.md").write_text(
        "See docs/VERITY_PIN.md for the pinned Verity revision.\n",
        encoding="utf-8",
      )
      (root / ".github/workflows/verify.yml").write_text(
        make_verify_workflow(),
        encoding="utf-8",
      )
      for rel in (
        "Morpho/Compiler/MacroSlice.lean",
        "Morpho/Proofs/SemanticBridgeReadiness.lean",
        "scripts/check_macro_migration_blockers.py",
      ):
        (root / rel).write_text("-- placeholder\n", encoding="utf-8")

      result = self.run_cli(
        root,
        "--lakefile",
        "lakefile.lean",
        "--manifest",
        "lake-manifest.json",
        "--provenance",
        "config/verity-pin-provenance.json",
        "--doc",
        "docs/VERITY_PIN.md",
        "--readme",
        "README.md",
        "--obligations",
        "config/semantic-bridge-obligations.json",
        "--workflow",
        ".github/workflows/verify.yml",
      )

    self.assertEqual(result.returncode, 1)
    self.assertIn("verity-pin-provenance check failed: failed to read documentation file", result.stderr)
    self.assertNotIn("Traceback", result.stderr)

  def test_cli_reports_missing_workflow_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      (root / "config").mkdir()
      (root / "docs").mkdir()
      (root / "Morpho/Compiler").mkdir(parents=True)
      (root / "scripts").mkdir()
      (root / "lakefile.lean").write_text(
        textwrap.dedent(
          """
          require verity from git
            "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
          """
        ),
        encoding="utf-8",
      )
      (root / "lake-manifest.json").write_text(
        textwrap.dedent(
          """
          {
            "packages": [
              {
                "name": "verity",
                "url": "https://github.com/Th0rgal/verity.git",
                "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
                "inputRev": "ad03fc64"
              }
            ]
          }
          """
        ),
        encoding="utf-8",
      )
      (root / "config/verity-pin-provenance.json").write_text(
        textwrap.dedent(
          """
          {
            "upstreamRepo": "https://github.com/Th0rgal/verity.git",
            "inputRev": "ad03fc64",
            "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
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
          """
        ),
        encoding="utf-8",
      )
      (root / "config/semantic-bridge-obligations.json").write_text(
        '{"issueClusters":[{"issue":123,"title":"Track migration gap"}]}',
        encoding="utf-8",
      )
      (root / "docs/VERITY_PIN.md").write_text(
        textwrap.dedent(
          """
          # Verity Pin

          - Repo: `https://github.com/Th0rgal/verity.git`
          - Short rev: `ad03fc64`
          - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
          - Tracking issue: `#118`

          ## Why this pin

          Current deterministic base.

          ## Remaining repo-local divergence at this pin

          ### Local generated-contract boundary

          Repo-local generated boundary remains.

          Relevant files:
          - `Morpho/Compiler/MacroSlice.lean`

          ## Enforcement

          The machine-readable source of truth is
          `config/verity-pin-provenance.json`. CI checks that it stays in sync with
          `lakefile.lean` and `lake-manifest.json` via
          `scripts/check_verity_pin_provenance.py`.
          """
        ),
        encoding="utf-8",
      )
      (root / "README.md").write_text(
        "See docs/VERITY_PIN.md for the pinned Verity revision.\n",
        encoding="utf-8",
      )
      (root / "Morpho/Compiler/MacroSlice.lean").write_text("-- placeholder\n", encoding="utf-8")

      result = self.run_cli(
        root,
        "--lakefile",
        "lakefile.lean",
        "--manifest",
        "lake-manifest.json",
        "--provenance",
        "config/verity-pin-provenance.json",
        "--doc",
        "docs/VERITY_PIN.md",
        "--readme",
        "README.md",
        "--obligations",
        "config/semantic-bridge-obligations.json",
        "--workflow",
        ".github/workflows/missing.yml",
      )

    self.assertEqual(result.returncode, 1)
    self.assertIn("verity-pin-provenance check failed: failed to read workflow file", result.stderr)
    self.assertNotIn("Traceback", result.stderr)

  def test_cli_accepts_relative_external_inputs_from_different_working_directory(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      workspace = pathlib.Path(d)
      external = workspace / "external"
      runner = workspace / "runner" / "nested"
      (external / "config").mkdir(parents=True)
      (external / "docs").mkdir()
      (external / ".github/workflows").mkdir(parents=True)
      (external / "Morpho/Compiler").mkdir(parents=True)
      (external / "Morpho/Proofs").mkdir(parents=True)
      (external / "scripts").mkdir()
      runner.mkdir(parents=True)

      (external / "lakefile.lean").write_text(
        textwrap.dedent(
          """
          require verity from git
            "https://github.com/Th0rgal/verity.git" @ "ad03fc64"
          """
        ),
        encoding="utf-8",
      )
      (external / "lake-manifest.json").write_text(
        textwrap.dedent(
          """
          {
            "packages": [
              {
                "name": "verity",
                "url": "https://github.com/Th0rgal/verity.git",
                "rev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
                "inputRev": "ad03fc64"
              }
            ]
          }
          """
        ),
        encoding="utf-8",
      )
      (external / "config/verity-pin-provenance.json").write_text(
        textwrap.dedent(
          """
          {
            "upstreamRepo": "https://github.com/Th0rgal/verity.git",
            "inputRev": "ad03fc64",
            "fullRev": "ad03fc64ed0e390e9d8c72f7cd469397324cda3a",
            "trackedIssue": "#118",
            "whyPinned": "Current deterministic base.",
            "remainingDivergences": [
              {
                "area": "Upstream macro/frontend gaps still block operation migration",
                "summary": "Repo-local macro migration blockers remain documented at this pin.",
                "issueClusters": [
                  "#123"
                ],
                "blockers": [
                  "callbacks"
                ],
                "files": [
                  "Morpho/Compiler/MacroSlice.lean",
                  "Morpho/Proofs/SemanticBridgeReadiness.lean",
                  "scripts/check_macro_migration_blockers.py"
                ]
              }
            ]
          }
          """
        ),
        encoding="utf-8",
      )
      (external / "config/semantic-bridge-obligations.json").write_text(
        '{"issueClusters":[{"issue":123,"title":"Track migration gap"}]}',
        encoding="utf-8",
      )
      (external / "docs/VERITY_PIN.md").write_text(
        textwrap.dedent(
          """
          # Verity Pin

          - Repo: `https://github.com/Th0rgal/verity.git`
          - Short rev: `ad03fc64`
          - Full rev: `ad03fc64ed0e390e9d8c72f7cd469397324cda3a`
          - Tracking issue: `#118`

          ## Why this pin

          Current deterministic base.

          ## Remaining repo-local divergence at this pin

          ### Upstream macro/frontend gaps still block operation migration

          Repo-local macro migration blockers remain documented at this pin.

          Current blocker families at this pin:
          - callbacks

          Tracked migration issue clusters:
          - #123

          Relevant files:
          - `Morpho/Compiler/MacroSlice.lean`
          - `Morpho/Proofs/SemanticBridgeReadiness.lean`
          - `scripts/check_macro_migration_blockers.py`

          ## Enforcement

          The machine-readable source of truth is
          `config/verity-pin-provenance.json`. CI checks that it stays in sync with
          `lakefile.lean` and `lake-manifest.json` via
          `scripts/check_verity_pin_provenance.py`.
          """
        ).strip() + "\n",
        encoding="utf-8",
      )
      (external / "README.md").write_text(
        "See docs/VERITY_PIN.md for the pinned Verity revision.\n",
        encoding="utf-8",
      )
      (external / ".github/workflows/verify.yml").write_text(
        make_verify_workflow(),
        encoding="utf-8",
      )
      for rel in (
        "Morpho/Compiler/MacroSlice.lean",
        "Morpho/Compiler/Generated.lean",
        "Morpho/Compiler/AdminAdapters.lean",
        "Morpho/Proofs/SemanticBridgeDischarge.lean",
        "Morpho/Proofs/SemanticBridgeInstantiation.lean",
        "Morpho/Proofs/SemanticBridgeReadiness.lean",
        "scripts/check_macro_migration_blockers.py",
      ):
        (external / rel).write_text("-- placeholder\n", encoding="utf-8")

      result = self.run_cli(
        runner,
        "--lakefile",
        "../../external/lakefile.lean",
        "--manifest",
        "../../external/lake-manifest.json",
        "--provenance",
        "../../external/config/verity-pin-provenance.json",
        "--doc",
        "../../external/docs/VERITY_PIN.md",
        "--readme",
        "../../external/README.md",
        "--obligations",
        "../../external/config/semantic-bridge-obligations.json",
        "--workflow",
        "../../external/.github/workflows/verify.yml",
      )

    self.assertEqual(result.returncode, 0)
    self.assertIn("verity-pin-provenance check: OK", result.stdout)

  def test_cli_no_arg_defaults_work_from_different_working_directory(self) -> None:
    runner = pathlib.Path(__file__).resolve().parent / "_tmp_runner"
    runner.mkdir(exist_ok=True)
    self.addCleanup(lambda: runner.rmdir())

    result = self.run_cli(runner)

    self.assertEqual(result.returncode, 0)
    self.assertIn("verity-pin-provenance check: OK", result.stdout)
