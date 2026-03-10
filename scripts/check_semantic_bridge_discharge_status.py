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
  "Links 2+3 are already provided upstream for the supported fragment (verity#1060 / #1065).\n"
  "The remaining blockers here are Link 1 discharge and macro frontend coverage\n"
  "for complex Morpho operations.\n"
  "Verity pin: 7b7c9193 (including linked externals, direct ERC20 helper syntax, "
  "and the executable tuple / struct / ecrecover macro surface now consumed by Morpho)."
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
EXPECTED_REMAINING_ROW = "| 5 | 10 remaining ops | blocked on macro / Link 1 | blocked |"
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
TRACKED_NAMESPACE_PATTERNS = (
  re.compile(r"(?m)^\s*/-!\s*## Discharge Status\s*$"),
  re.compile(re.escape(EXPECTED_DISCHARGE_STATUS), re.DOTALL),
  re.compile(re.escape(EXPECTED_FLASHLOAN_ROW), re.DOTALL),
)


class SemanticBridgeDischargeStatusError(RuntimeError):
  pass


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except FileNotFoundError as exc:
    raise SemanticBridgeDischargeStatusError(f"failed to read {path}: {exc}") from exc
  except OSError as exc:
    raise SemanticBridgeDischargeStatusError(f"failed to read {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise SemanticBridgeDischargeStatusError(f"failed to read {path}: {exc}") from exc


def normalize_text(text: str) -> str:
  return re.sub(r"\s+", " ", text.replace("`", "")).strip()


def require_unique_line(text: str, line: str, error: str) -> re.Match[str]:
  matches = list(re.finditer(rf"(?m)^\s*{re.escape(line)}\r?$", text))
  if len(matches) != 1:
    raise SemanticBridgeDischargeStatusError(error)
  return matches[0]


def extract_namespace_blocks(text: str) -> list[tuple[re.Match[str], re.Match[str], str]]:
  namespace_matches = list(re.finditer(rf"(?m)^\s*{re.escape(NAMESPACE_HEADER)}\r?$", text))
  if not namespace_matches:
    raise SemanticBridgeDischargeStatusError(
      "SemanticBridgeDischarge.lean status drift: missing namespace header"
    )
  end_matches = list(re.finditer(rf"(?m)^\s*{re.escape(NAMESPACE_FOOTER)}\r?$", text))
  if not end_matches:
    raise SemanticBridgeDischargeStatusError(
      "SemanticBridgeDischarge.lean status drift: missing namespace footer"
    )
  if end_matches[0].start() < namespace_matches[0].start():
    raise SemanticBridgeDischargeStatusError(
      "SemanticBridgeDischarge.lean status drift: unmatched namespace footer before first namespace block"
    )
  blocks: list[tuple[re.Match[str], re.Match[str], str]] = []
  end_index = 0
  previous_end = -1
  for namespace_match in namespace_matches:
    if namespace_match.start() < previous_end:
      raise SemanticBridgeDischargeStatusError(
        "SemanticBridgeDischarge.lean status drift: namespace blocks overlap"
      )
    while end_index < len(end_matches) and end_matches[end_index].start() <= namespace_match.end():
      end_index += 1
    if end_index >= len(end_matches):
      raise SemanticBridgeDischargeStatusError(
        "SemanticBridgeDischarge.lean status drift: missing namespace footer"
      )
    end_match = end_matches[end_index]
    namespace_body = text[namespace_match.end() : end_match.start()]
    if not namespace_body.strip():
      raise SemanticBridgeDischargeStatusError(
        "SemanticBridgeDischarge.lean status drift: empty namespace body"
      )
    blocks.append((namespace_match, end_match, namespace_body))
    previous_end = end_match.end()
    end_index += 1
  if end_index < len(end_matches):
    raise SemanticBridgeDischargeStatusError(
      "SemanticBridgeDischarge.lean status drift: unmatched namespace footer after namespace blocks"
    )
  return blocks


def namespace_block_has_tracked_status(namespace_body: str) -> bool:
  return any(pattern.search(namespace_body) is not None for pattern in TRACKED_NAMESPACE_PATTERNS)


def extract_namespace_block(text: str) -> str:
  namespace_blocks = extract_namespace_blocks(text)
  _, _, primary_namespace_body = namespace_blocks[0]
  if not namespace_block_has_tracked_status(primary_namespace_body):
    raise SemanticBridgeDischargeStatusError(
      "SemanticBridgeDischarge.lean status drift: primary namespace block is missing tracked status content"
    )
  duplicate_status_blocks = [
    match.group(0).strip()
    for match, _, namespace_body in namespace_blocks[1:]
    if namespace_block_has_tracked_status(namespace_body)
  ]
  if duplicate_status_blocks:
    raise SemanticBridgeDischargeStatusError(
      "SemanticBridgeDischarge.lean status drift: found multiple namespace blocks with tracked status content"
    )
  return primary_namespace_body


def extract_intro_block(text: str) -> str:
  namespace_match = extract_namespace_blocks(text)[0][0]
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
    (discharge_text, EXPECTED_REMAINING_ROW),
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
  discharge_path = args.discharge.resolve()

  text = read_text(discharge_path)
  validate_status(text)

  print("semantic-bridge-discharge-status check: OK")
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except SemanticBridgeDischargeStatusError as e:
    print(f"semantic-bridge-discharge-status check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
