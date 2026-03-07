#!/usr/bin/env python3
"""Fail-closed sync check for the documented Verity pin provenance."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys

from workflow_run_parser import extract_named_step_runs
from typing import Any

from check_verity_pin_sync import parse_lakefile_verity, parse_manifest_verity

ROOT = pathlib.Path(__file__).resolve().parent.parent


MACRO_FRONTEND_AREA = "Upstream macro/frontend gaps still block operation migration"
MACRO_BLOCKERS_LABEL = "Current blocker families at this pin:"
MACRO_ISSUE_CLUSTERS_LABEL = "Tracked migration issue clusters:"
FILES_LABEL = "Relevant files:"
WHY_THIS_PIN_HEADING = "## Why this pin"
REMAINING_DIVERGENCES_HEADING = "## Remaining repo-local divergence at this pin"
ENFORCEMENT_HEADING = "## Enforcement"
SECTION_LIST_LABELS = {
  FILES_LABEL,
  MACRO_BLOCKERS_LABEL,
  MACRO_ISSUE_CLUSTERS_LABEL,
}
ALLOWED_METADATA_PREAMBLE = "morpho-verity currently pins Verity to:"
EXPECTED_ENFORCEMENT_TEXT = (
  "The machine-readable source of truth is `config/verity-pin-provenance.json`. "
  "CI checks that it stays in sync with `lakefile.lean` and `lake-manifest.json` "
  "via `scripts/check_verity_pin_provenance.py`."
)
EXPECTED_WORKFLOW_STEPS = (
  "Validate verity pin sync",
  "Validate verity pin provenance",
)
EXPECTED_WORKFLOW_RUN_LINES = {
  "Validate verity pin sync":
    './scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "Validate verity pin sync" -- python3 scripts/check_verity_pin_sync.py',
  "Validate verity pin provenance":
    './scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "Validate verity pin provenance" -- python3 scripts/check_verity_pin_provenance.py',
}


def fail(msg: str) -> None:
  print(f"verity-pin-provenance check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path, *, context: str) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except (OSError, UnicodeDecodeError) as exc:
    fail(f"failed to read {context} {path}: {exc}")


def load_provenance(path: pathlib.Path) -> dict[str, object]:
  try:
    data = json.loads(read_text(path, context="provenance file"))
  except json.JSONDecodeError as exc:
    fail(f"failed to parse provenance JSON {path}: {exc}")
  if not isinstance(data, dict):
    fail(f"provenance file must be a JSON object: {path}")
  return data


def load_issue_clusters(path: pathlib.Path) -> dict[str, dict[str, Any]]:
  try:
    data = json.loads(read_text(path, context="issue cluster file"))
  except json.JSONDecodeError as exc:
    fail(f"failed to parse issue cluster JSON {path}: {exc}")
  if not isinstance(data, dict):
    fail(f"obligations file must be a JSON object: {path}")
  raw_clusters = data.get("issueClusters")
  if not isinstance(raw_clusters, list) or not raw_clusters:
    fail(f"missing non-empty `issueClusters` list in {path}")

  clusters: dict[str, dict[str, Any]] = {}
  for i, item in enumerate(raw_clusters):
    if not isinstance(item, dict):
      fail(f"`issueClusters[{i}]` must be an object in {path}")
    issue = item.get("issue")
    title = item.get("title")
    if isinstance(issue, bool) or not isinstance(issue, int):
      fail(f"`issueClusters[{i}].issue` must be an integer in {path}")
    if not isinstance(title, str) or not title:
      fail(f"`issueClusters[{i}].title` must be a non-empty string in {path}")
    issue_key = f"#{issue}"
    if issue_key in clusters:
      fail(f"duplicate `issueClusters[{i}].issue` value {issue_key} in {path}")
    clusters[issue_key] = item
  return clusters


def require_str(data: dict[str, object], key: str, path: pathlib.Path) -> str:
  value = data.get(key)
  if not isinstance(value, str) or not value:
    fail(f"missing non-empty string `{key}` in {path}")
  return value


def require_divergences(data: dict[str, object], path: pathlib.Path) -> list[dict[str, object]]:
  value = data.get("remainingDivergences")
  if not isinstance(value, list) or not value:
    fail(f"missing non-empty `remainingDivergences` list in {path}")
  result: list[dict[str, object]] = []
  for i, item in enumerate(value):
    if not isinstance(item, dict):
      fail(f"`remainingDivergences[{i}]` must be an object in {path}")
    area = item.get("area")
    summary = item.get("summary")
    files = item.get("files")
    blockers = item.get("blockers")
    issue_clusters = item.get("issueClusters")
    if not isinstance(area, str) or not area:
      fail(f"`remainingDivergences[{i}].area` must be a non-empty string in {path}")
    if not isinstance(summary, str) or not summary:
      fail(f"`remainingDivergences[{i}].summary` must be a non-empty string in {path}")
    if not isinstance(files, list) or not files or not all(isinstance(f, str) and f for f in files):
      fail(f"`remainingDivergences[{i}].files` must be a non-empty string list in {path}")
    if blockers is not None and (
      not isinstance(blockers, list)
      or not blockers
      or not all(isinstance(b, str) and b for b in blockers)
    ):
      fail(
        f"`remainingDivergences[{i}].blockers` must be a non-empty string list in {path}"
      )
    if area == MACRO_FRONTEND_AREA and blockers is None:
      fail(
        f"`remainingDivergences[{i}].blockers` is required for `{MACRO_FRONTEND_AREA}` in {path}"
      )
    if issue_clusters is not None and (
      not isinstance(issue_clusters, list)
      or not issue_clusters
      or not all(isinstance(issue, str) and re.fullmatch(r"#\d+", issue) for issue in issue_clusters)
    ):
      fail(
        f"`remainingDivergences[{i}].issueClusters` must be a non-empty #123-style string list in {path}"
      )
    if area == MACRO_FRONTEND_AREA and issue_clusters is None:
      fail(
        f"`remainingDivergences[{i}].issueClusters` is required for `{MACRO_FRONTEND_AREA}` in {path}"
      )
    result.append(item)
  return result


def require_doc_mentions(doc_text: str, needle: str, doc_path: pathlib.Path) -> None:
  normalized_doc = re.sub(r"\s+", " ", doc_text.replace("`", "")).strip()
  normalized_needle = re.sub(r"\s+", " ", needle.replace("`", "")).strip()
  if normalized_needle not in normalized_doc:
    fail(f"documentation {doc_path} missing expected text: {needle}")


def normalize_doc_token(text: str) -> str:
  return re.sub(r"\s+", " ", text.replace("`", "")).strip()


def require_unique_heading_line(doc_text: str, heading: str, doc_path: pathlib.Path) -> re.Match[str]:
  matches = list(re.finditer(rf"(?m)^{re.escape(heading)}\r?$", doc_text))
  if not matches:
    fail(f"documentation {doc_path} missing expected heading: {heading}")
  if len(matches) > 1:
    fail(f"documentation {doc_path} has duplicate heading: {heading}")
  return matches[0]


def extract_heading_section(
  doc_text: str,
  heading: str,
  *,
  doc_path: pathlib.Path,
  next_heading: str | None = None,
) -> str:
  start_match = require_unique_heading_line(doc_text, heading, doc_path)
  if next_heading is None:
    section_text = doc_text[start_match.end() :]
  else:
    end_match = require_unique_heading_line(doc_text, next_heading, doc_path)
    if end_match.start() <= start_match.end():
      fail(
        f"documentation {doc_path} has invalid heading boundary ordering: "
        f"{heading} before {next_heading}"
      )
    section_text = doc_text[start_match.end() : end_match.start()]
  return section_text


def extract_section_until_next_h2(
  doc_text: str,
  heading: str,
  *,
  doc_path: pathlib.Path,
) -> str:
  start_match = require_unique_heading_line(doc_text, heading, doc_path)
  next_h2_match = re.search(r"(?m)^## .*\r?$", doc_text[start_match.end() :])
  if next_h2_match is None:
    return doc_text[start_match.end() :]
  return doc_text[start_match.end() : start_match.end() + next_h2_match.start()]


def extract_markdown_section(doc_text: str, heading: str, doc_path: pathlib.Path) -> str:
  marker = f"### {heading}"
  divergence_section = extract_heading_section(
    doc_text,
    REMAINING_DIVERGENCES_HEADING,
    doc_path=doc_path,
    next_heading=ENFORCEMENT_HEADING,
  )
  lines = divergence_section.splitlines()
  matches = [index for index, line in enumerate(lines) if line.strip() == marker]
  if not matches:
    fail(f"documentation {doc_path} missing expected section heading: {marker}")
  if len(matches) > 1:
    fail(f"documentation {doc_path} has duplicate section heading: {marker}")
  start_index = matches[0] + 1

  section_lines: list[str] = []
  for line in lines[start_index:]:
    stripped = line.strip()
    if stripped.startswith("## ") or stripped.startswith("### "):
      break
    section_lines.append(line)
  return "\n".join(section_lines)


def extract_doc_lead_bullets(doc_text: str, stop_heading: str, doc_path: pathlib.Path) -> list[str]:
  start_match = require_unique_heading_line(doc_text, "# Verity Pin", doc_path)
  end_match = require_unique_heading_line(doc_text, stop_heading, doc_path)
  if end_match.start() <= start_match.end():
    fail(
      f"documentation {doc_path} has invalid heading boundary ordering: "
      f"# Verity Pin before {stop_heading}"
    )

  lines = doc_text[start_match.end() : end_match.start()].splitlines()
  bullets: list[str] = []
  for line in lines:
    stripped = line.strip()
    if not stripped:
      continue
    if not bullets and not stripped.startswith("- "):
      if normalize_doc_token(stripped) != normalize_doc_token(ALLOWED_METADATA_PREAMBLE) and ":" in stripped:
        fail(
          f"documentation {doc_path} has stale Verity pin metadata before bullet list: {stripped}"
        )
      continue
    if not stripped.startswith("- "):
      fail(f"documentation {doc_path} has malformed Verity pin metadata line: {stripped}")
    bullets.append(stripped[2:].strip())
  if not bullets:
    fail(f"documentation {doc_path} missing Verity pin metadata bullets")
  return bullets


def extract_section_body(doc_text: str, heading: str, doc_path: pathlib.Path) -> str:
  next_heading = {
    WHY_THIS_PIN_HEADING: REMAINING_DIVERGENCES_HEADING,
    REMAINING_DIVERGENCES_HEADING: ENFORCEMENT_HEADING,
  }.get(heading)
  if next_heading is None and heading == ENFORCEMENT_HEADING:
    return extract_section_until_next_h2(doc_text, heading, doc_path=doc_path)
  return extract_heading_section(
    doc_text,
    heading,
    doc_path=doc_path,
    next_heading=next_heading,
  )


def extract_section_subheadings(
  doc_text: str,
  heading: str,
  *,
  subheading_prefix: str,
  doc_path: pathlib.Path,
) -> list[str]:
  section_text = extract_section_body(doc_text, heading, doc_path)
  subheadings: list[str] = []
  prefix = f"{subheading_prefix} "
  for line in section_text.splitlines():
    stripped = line.strip()
    if stripped.startswith(prefix):
      subheadings.append(stripped[len(prefix):].strip())
  return subheadings


def extract_summary_text(section_text: str) -> str:
  summary_lines: list[str] = []
  for line in section_text.splitlines():
    stripped = line.strip()
    if not stripped:
      if summary_lines:
        summary_lines.append("")
      continue
    if stripped in SECTION_LIST_LABELS:
      break
    if stripped.startswith("#"):
      break
    summary_lines.append(line)
  return "\n".join(summary_lines).strip()


def build_expected_section_lists(item: dict[str, object]) -> list[tuple[str, list[str]]]:
  expected_lists: list[tuple[str, list[str]]] = []

  blockers = item.get("blockers")
  if isinstance(blockers, list):
    expected_lists.append((MACRO_BLOCKERS_LABEL, [str(blocker) for blocker in blockers]))

  issue_clusters = item.get("issueClusters")
  if isinstance(issue_clusters, list):
    expected_lists.append((MACRO_ISSUE_CLUSTERS_LABEL, [str(issue) for issue in issue_clusters]))

  files = item.get("files")
  if isinstance(files, list):
    expected_lists.append((FILES_LABEL, [str(file_path) for file_path in files]))

  return expected_lists


def parse_section_lists(
  section_text: str,
  doc_path: pathlib.Path,
) -> list[tuple[str, list[str]]]:
  lines = section_text.splitlines()
  lists: list[tuple[str, list[str]]] = []
  seen_labels: set[str] = set()
  in_summary = True
  index = 0

  while index < len(lines):
    stripped = lines[index].strip()
    if in_summary:
      if not stripped:
        index += 1
        continue
      if stripped in SECTION_LIST_LABELS:
        in_summary = False
      else:
        index += 1
        continue

    if not stripped:
      index += 1
      continue
    if stripped.startswith("#"):
      break
    if stripped not in SECTION_LIST_LABELS:
      fail(f"documentation {doc_path} has unexpected content in divergence section: {stripped}")
    if stripped in seen_labels:
      fail(f"documentation {doc_path} has duplicate list label: {stripped}")

    label = stripped
    seen_labels.add(label)
    index += 1
    bullets: list[str] = []
    while index < len(lines):
      stripped = lines[index].strip()
      if not stripped:
        index += 1
        if bullets:
          break
        continue
      if stripped in SECTION_LIST_LABELS or stripped.startswith("#"):
        break
      if not stripped.startswith("- "):
        fail(f"documentation {doc_path} has malformed bullet under `{label}`: {stripped}")
      bullets.append(stripped[2:].strip())
      index += 1
    if not bullets:
      fail(f"documentation {doc_path} missing bullets under `{label}`")
    lists.append((label, bullets))

  return lists

def validate_doc_section(
  section_text: str,
  item: dict[str, object],
  doc_path: pathlib.Path,
) -> None:
  actual_summary = normalize_doc_token(extract_summary_text(section_text))
  expected_summary = normalize_doc_token(str(item["summary"]))
  if actual_summary != expected_summary:
    fail(
      f"documentation {doc_path} `{item['area']}` summary drift: "
      f"expected {expected_summary!r}; found {actual_summary!r}"
    )

  expected_lists = build_expected_section_lists(item)
  actual_lists = parse_section_lists(section_text, doc_path)
  actual_labels = [label for label, _ in actual_lists]
  expected_labels = [label for label, _ in expected_lists]
  if actual_labels != expected_labels:
    fail(
      f"documentation {doc_path} `{item['area']}` section label drift: "
      f"expected {expected_labels}; found {actual_labels}"
    )
  for (label, actual_items_raw), (_, expected_items) in zip(actual_lists, expected_lists):
    actual_items = [normalize_doc_token(item_text) for item_text in actual_items_raw]
    normalized_expected_items = [normalize_doc_token(item_text) for item_text in expected_items]
    if actual_items != normalized_expected_items:
      fail(
        f"documentation {doc_path} `{item['area']}` `{label}` drift: "
        f"expected {normalized_expected_items}; found {actual_items}"
      )


def validate_doc_metadata(
  *,
  doc_text: str,
  upstream_repo: str,
  input_rev: str,
  full_rev: str,
  tracked_issue: str,
  doc_path: pathlib.Path,
) -> None:
  actual = [
    normalize_doc_token(item)
    for item in extract_doc_lead_bullets(doc_text, WHY_THIS_PIN_HEADING, doc_path)
  ]
  expected = [
    normalize_doc_token(f"Repo: {upstream_repo}"),
    normalize_doc_token(f"Short rev: {input_rev}"),
    normalize_doc_token(f"Full rev: {full_rev}"),
    normalize_doc_token(f"Tracking issue: {tracked_issue}"),
  ]
  if actual != expected:
    fail(
      f"documentation {doc_path} Verity pin metadata drift: expected {expected}; found {actual}"
    )


def validate_why_pinned(
  *,
  doc_text: str,
  why_pinned: str,
  doc_path: pathlib.Path,
) -> None:
  actual = normalize_doc_token(
    extract_section_body(doc_text, WHY_THIS_PIN_HEADING, doc_path)
  )
  expected = normalize_doc_token(why_pinned)
  if actual != expected:
    fail(
      f"documentation {doc_path} why-pinned summary drift: "
      f"expected {expected!r}; found {actual!r}"
    )


def validate_enforcement_section(
  *,
  doc_text: str,
  doc_path: pathlib.Path,
) -> None:
  actual = normalize_doc_token(extract_section_body(doc_text, ENFORCEMENT_HEADING, doc_path))
  expected = normalize_doc_token(EXPECTED_ENFORCEMENT_TEXT)
  if actual != expected:
    fail(
      f"documentation {doc_path} enforcement summary drift: "
      f"expected {expected!r}; found {actual!r}"
    )


def validate_divergence_section_headings(
  *,
  doc_text: str,
  divergences: list[dict[str, object]],
  doc_path: pathlib.Path,
) -> None:
  actual = [
    normalize_doc_token(item)
    for item in extract_section_subheadings(
      doc_text,
      REMAINING_DIVERGENCES_HEADING,
      subheading_prefix="###",
      doc_path=doc_path,
    )
  ]
  expected = [normalize_doc_token(str(item["area"])) for item in divergences]
  if actual != expected:
    fail(
      f"documentation {doc_path} divergence section heading drift: "
      f"expected {expected}; found {actual}"
    )


def validate_workflow(
  *,
  workflow_text: str,
  workflow_path: pathlib.Path,
) -> None:
  step_name_counts, step_run_lines = extract_named_step_runs(workflow_text)
  missing = [step for step in EXPECTED_WORKFLOW_STEPS if step_name_counts.get(step, 0) == 0]
  if missing:
    fail(
      f"workflow {workflow_path} missing Verity pin enforcement step(s): {', '.join(missing)}"
    )
  duplicate = [step for step in EXPECTED_WORKFLOW_STEPS if step_name_counts.get(step, 0) > 1]
  if duplicate:
    fail(
      f"workflow {workflow_path} duplicates Verity pin enforcement step(s): {', '.join(duplicate)}"
    )
  mismatched_run = []
  for step, expected_run_line in EXPECTED_WORKFLOW_RUN_LINES.items():
    actual_run_lines = step_run_lines.get(step, [])
    if actual_run_lines != [expected_run_line]:
      mismatched_run.append(step)
  if mismatched_run:
    fail(
      "workflow "
      f"{workflow_path} has drifted Verity pin enforcement command(s): {', '.join(mismatched_run)}"
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate the documented Verity pin provenance against lake metadata"
  )
  parser.add_argument(
    "--lakefile",
    type=pathlib.Path,
    default=ROOT / "lakefile.lean",
    help="Path to lakefile.lean",
  )
  parser.add_argument(
    "--manifest",
    type=pathlib.Path,
    default=ROOT / "lake-manifest.json",
    help="Path to lake-manifest.json",
  )
  parser.add_argument(
    "--provenance",
    type=pathlib.Path,
    default=ROOT / "config/verity-pin-provenance.json",
    help="Path to the machine-readable provenance file",
  )
  parser.add_argument(
    "--doc",
    type=pathlib.Path,
    default=ROOT / "docs/VERITY_PIN.md",
    help="Path to the human-readable provenance document",
  )
  parser.add_argument(
    "--readme",
    type=pathlib.Path,
    default=ROOT / "README.md",
    help="Path to README",
  )
  parser.add_argument(
    "--obligations",
    type=pathlib.Path,
    default=ROOT / "config/semantic-bridge-obligations.json",
    help="Path to the semantic-bridge obligation tracker",
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=ROOT / ".github/workflows/verify.yml",
    help="Path to the verify workflow",
  )
  args = parser.parse_args()

  lakefile_path = args.lakefile.resolve()
  manifest_path = args.manifest.resolve()
  provenance_path = args.provenance.resolve()
  doc_path = args.doc.resolve()
  readme_path = args.readme.resolve()
  obligations_path = args.obligations.resolve()
  workflow_path = args.workflow.resolve()

  lakefile_url, lakefile_rev = parse_lakefile_verity(lakefile_path)
  manifest_url, manifest_rev, manifest_input_rev = parse_manifest_verity(manifest_path)
  provenance = load_provenance(provenance_path)
  issue_clusters = load_issue_clusters(obligations_path)

  upstream_repo = require_str(provenance, "upstreamRepo", provenance_path)
  input_rev = require_str(provenance, "inputRev", provenance_path)
  full_rev = require_str(provenance, "fullRev", provenance_path)
  tracked_issue = require_str(provenance, "trackedIssue", provenance_path)
  why_pinned = require_str(provenance, "whyPinned", provenance_path)
  divergences = require_divergences(provenance, provenance_path)

  if upstream_repo != lakefile_url or upstream_repo != manifest_url:
    fail(
      "provenance upstreamRepo does not match lake metadata: "
      f"{upstream_repo} / {lakefile_url} / {manifest_url}"
    )
  if input_rev != lakefile_rev or input_rev != manifest_input_rev:
    fail(
      "provenance inputRev does not match lake metadata: "
      f"{input_rev} / {lakefile_rev} / {manifest_input_rev}"
    )
  if full_rev != manifest_rev:
    fail(f"provenance fullRev does not match manifest rev: {full_rev} != {manifest_rev}")
  if not full_rev.startswith(input_rev):
    fail(f"provenance fullRev does not start with inputRev: {full_rev} / {input_rev}")

  repo_root = provenance_path.parent.parent
  for item in divergences:
    for raw_path in item["files"]:
      file_path = repo_root / raw_path
      if not file_path.exists():
        fail(f"documented divergence file does not exist: {raw_path}")

  doc_text = read_text(doc_path, context="documentation file")
  validate_doc_metadata(
    doc_text=doc_text,
    upstream_repo=upstream_repo,
    input_rev=input_rev,
    full_rev=full_rev,
    tracked_issue=tracked_issue,
    doc_path=doc_path,
  )
  validate_why_pinned(
    doc_text=doc_text,
    why_pinned=why_pinned,
    doc_path=doc_path,
  )
  validate_divergence_section_headings(
    doc_text=doc_text,
    divergences=divergences,
    doc_path=doc_path,
  )
  validate_enforcement_section(doc_text=doc_text, doc_path=doc_path)
  require_doc_mentions(doc_text, upstream_repo, doc_path)
  require_doc_mentions(doc_text, input_rev, doc_path)
  require_doc_mentions(doc_text, full_rev, doc_path)
  require_doc_mentions(doc_text, tracked_issue, doc_path)
  require_doc_mentions(doc_text, why_pinned, doc_path)
  for item in divergences:
    require_doc_mentions(doc_text, item["area"], doc_path)
    require_doc_mentions(doc_text, item["summary"], doc_path)
    section_text = extract_markdown_section(doc_text, item["area"], doc_path)
    validate_doc_section(section_text, item, doc_path)
    blockers = item.get("blockers")
    if isinstance(blockers, list):
      for blocker in blockers:
        require_doc_mentions(doc_text, blocker, doc_path)
    tracked_issue_clusters = item.get("issueClusters")
    if isinstance(tracked_issue_clusters, list):
      for issue in tracked_issue_clusters:
        if issue not in issue_clusters:
          fail(
            f"documented divergence references unknown issue cluster {issue}: {obligations_path}"
          )
        require_doc_mentions(doc_text, issue, doc_path)
    for raw_path in item["files"]:
      require_doc_mentions(doc_text, raw_path, doc_path)

  readme_text = read_text(readme_path, context="README file")
  require_doc_mentions(readme_text, "docs/VERITY_PIN.md", readme_path)
  workflow_text = read_text(workflow_path, context="workflow file")
  validate_workflow(workflow_text=workflow_text, workflow_path=workflow_path)

  print(
    "verity-pin-provenance: "
    f"input_rev={input_rev} full_rev={full_rev} divergences={len(divergences)}"
  )
  print("verity-pin-provenance check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
