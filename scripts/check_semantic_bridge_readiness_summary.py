#!/usr/bin/env python3
"""Fail-closed sync check for the proof-facing summary in SemanticBridgeReadiness.lean."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys
from typing import Any

from check_semantic_bridge_readiness_sync import (
  SemanticBridgeReadinessSyncError,
  build_config_projection,
  load_config,
)


ROOT = pathlib.Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
READINESS_PATH = ROOT / "Morpho" / "Proofs" / "SemanticBridgeReadiness.lean"
DISCHARGE_PATH_PREFIX = (
  "The upstream Verity semantic bridge now provides, for each supported function\n"
  "`f` in a `verity_contract`:"
)
DISCHARGE_PATH_HEADER_PATTERN = re.compile(
  r"(?m)^\s*##\s+Discharge\s+Path\s+\(verity#1060\s+/\s+#1065\)\s*$"
)
OBLIGATION_REGISTRY_HEADER_PATTERN = re.compile(
  r"(?m)^\s*##\s+Obligation\s+Registry\s*$"
)
NAMESPACE_PATTERN = re.compile(
  r"(?m)^\s*namespace\s+Morpho\.Proofs\.SemanticBridgeReadiness\s*$"
)
END_NAMESPACE_PATTERN = re.compile(
  r"(?m)^\s*end\s+Morpho\.Proofs\.SemanticBridgeReadiness\s*$"
)
INTRO_DOCBLOCK_PATTERN = re.compile(r"\A\s*/-!(?P<body>.*?)-/", re.DOTALL)


class SemanticBridgeReadinessSummaryError(RuntimeError):
  pass


TOTAL_COMMENT_RE = re.compile(
  r"/--\s+All\s+(?P<count>\d+)\s+semantic equivalence obligations from SolidityBridge\.lean\.",
  re.DOTALL,
)
LINK1_COMMENT_RE = re.compile(
  r"/--\s+(?P<count>\d+)\s+of\s+(?P<total>\d+)\s+operations have Link 1 proven\.",
  re.DOTALL,
)
LINK1_LIST_RE = re.compile(
  r"These\s+(?P<count>\d+)\s+Link 1 operations are:\s+(?P<ops>[^.]+)\.",
  re.DOTALL,
)
ASSUMED_COMMENT_RE = re.compile(
  r"/--\s+(?P<count>\d+)\s+operations still have assumed status \(Link 1 not yet proven\)\.\s+-/",
  re.DOTALL,
)
MIGRATED_COMMENT_RE = re.compile(
  r"/--\s+(?P<count>\d+)\s+of\s+(?P<total>\d+)\s+operations have full \(non-stub\) macro implementations\.",
  re.DOTALL,
)
PENDING_COMMENT_RE = re.compile(
  r"/--\s+(?P<count>\d+)\s+operations still need macro migration before discharge\.\s+-/",
  re.DOTALL,
)
THEOREM_PATTERNS = {
  "obligation_count": re.compile(
    r"theorem\s+obligation_count\s*:\s*obligations\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
  "link1_proven_count": re.compile(
    r"theorem\s+link1_proven_count\s*:\s*\(obligations\.filter\s*\(fun o => o\.status != \.assumed\)\)\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
  "assumed_count": re.compile(
    r"theorem\s+assumed_count\s*:\s*\(obligations\.filter\s*\(fun o => o\.status == \.assumed\)\)\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
  "macro_migrated_count": re.compile(
    r"theorem\s+macro_migrated_count\s*:\s*\(obligations\.filter\s*\(fun o => o\.macroMigrated\)\)\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
  "macro_pending_count": re.compile(
    r"theorem\s+macro_pending_count\s*:\s*\(obligations\.filter\s*\(fun o => !o\.macroMigrated\)\)\.length\s*=\s*(?P<count>\d+)\s*:=\s*by",
    re.DOTALL,
  ),
}
TRACKED_NAMESPACE_PATTERNS = (
  TOTAL_COMMENT_RE,
  LINK1_COMMENT_RE,
  LINK1_LIST_RE,
  ASSUMED_COMMENT_RE,
  MIGRATED_COMMENT_RE,
  PENDING_COMMENT_RE,
  *THEOREM_PATTERNS.values(),
)


def derive_summary(config: dict[str, Any]) -> dict[str, Any]:
  entries = build_config_projection(load_config(config) if isinstance(config, pathlib.Path) else config)
  total = len(entries)
  link1_operations = [entry["operation"] for entry in entries if entry["status"] != "assumed"]
  assumed_count = sum(1 for entry in entries if entry["status"] == "assumed")
  macro_migrated_count = sum(1 for entry in entries if entry["macroMigrated"])
  return {
    "total": total,
    "link1_count": len(link1_operations),
    "link1_operations": link1_operations,
    "assumed_count": assumed_count,
    "macro_migrated_count": macro_migrated_count,
    "macro_pending_count": total - macro_migrated_count,
  }


def read_readiness_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise SemanticBridgeReadinessSummaryError(
      f"failed to read SemanticBridgeReadiness file {path}: {exc}"
    ) from exc
  except UnicodeDecodeError as exc:
    raise SemanticBridgeReadinessSummaryError(
      f"failed to decode SemanticBridgeReadiness file {path} as UTF-8: {exc}"
    ) from exc


def require_match(pattern: re.Pattern[str], text: str, description: str) -> re.Match[str]:
  match = pattern.search(text)
  if match is None:
    raise SemanticBridgeReadinessSummaryError(
      f"failed to locate {description} in SemanticBridgeReadiness.lean"
    )
  return match


def require_unique_match(
  pattern: re.Pattern[str],
  text: str,
  description: str,
) -> re.Match[str]:
  matches = list(pattern.finditer(text))
  if not matches:
    raise SemanticBridgeReadinessSummaryError(
      f"failed to locate {description} in SemanticBridgeReadiness.lean"
    )
  if len(matches) > 1:
    raise SemanticBridgeReadinessSummaryError(
      f"found multiple {description} matches in SemanticBridgeReadiness.lean"
    )
  return matches[0]


def require_first_match(
  pattern: re.Pattern[str],
  text: str,
  description: str,
  *,
  start: int = 0,
) -> re.Match[str]:
  match = pattern.search(text, start)
  if match is None:
    raise SemanticBridgeReadinessSummaryError(
      f"failed to locate {description} in SemanticBridgeReadiness.lean"
    )
  return match


def extract_namespace_blocks(text: str) -> list[tuple[re.Match[str], re.Match[str], str]]:
  namespace_matches = list(NAMESPACE_PATTERN.finditer(text))
  if not namespace_matches:
    raise SemanticBridgeReadinessSummaryError(
      "failed to locate SemanticBridgeReadiness namespace boundary in SemanticBridgeReadiness.lean"
    )

  end_matches = list(END_NAMESPACE_PATTERN.finditer(text))
  if not end_matches:
    raise SemanticBridgeReadinessSummaryError(
      "failed to locate SemanticBridgeReadiness namespace end boundary in SemanticBridgeReadiness.lean"
    )
  if end_matches[0].start() < namespace_matches[0].start():
    raise SemanticBridgeReadinessSummaryError(
      "found unmatched SemanticBridgeReadiness namespace footer before first namespace block"
    )

  blocks: list[tuple[re.Match[str], re.Match[str], str]] = []
  end_index = 0
  previous_end = -1
  for namespace_match in namespace_matches:
    if namespace_match.start() < previous_end:
      raise SemanticBridgeReadinessSummaryError(
        "SemanticBridgeReadiness namespace blocks overlap in SemanticBridgeReadiness.lean"
      )
    while end_index < len(end_matches) and end_matches[end_index].start() <= namespace_match.end():
      end_index += 1
    if end_index >= len(end_matches):
      raise SemanticBridgeReadinessSummaryError(
        "failed to locate SemanticBridgeReadiness namespace end boundary in SemanticBridgeReadiness.lean"
      )
    end_match = end_matches[end_index]
    namespace_body = text[namespace_match.end() : end_match.start()]
    if not namespace_body.strip():
      raise SemanticBridgeReadinessSummaryError("SemanticBridgeReadiness namespace body is empty")
    blocks.append((namespace_match, end_match, namespace_body))
    previous_end = end_match.end()
    end_index += 1

  if end_index != len(end_matches):
    raise SemanticBridgeReadinessSummaryError(
      "found unmatched SemanticBridgeReadiness namespace footer after namespace blocks"
    )
  return blocks


def namespace_block_has_tracked_summary(namespace_body: str) -> bool:
  return any(pattern.search(namespace_body) is not None for pattern in TRACKED_NAMESPACE_PATTERNS)


def extract_intro_section(text: str) -> str:
  namespace_blocks = extract_namespace_blocks(text)
  namespace_match = namespace_blocks[0][0]
  intro = text[: namespace_match.start()]
  if not intro.strip():
    raise SemanticBridgeReadinessSummaryError("SemanticBridgeReadiness intro section is empty")
  intro_docblock = INTRO_DOCBLOCK_PATTERN.match(intro)
  if intro_docblock is None:
    raise SemanticBridgeReadinessSummaryError(
      "SemanticBridgeReadiness intro section must begin with a closed module docblock"
    )
  trailing_intro = intro[intro_docblock.end() :]
  if "/-!" in trailing_intro:
    raise SemanticBridgeReadinessSummaryError(
      "SemanticBridgeReadiness intro section contains multiple module docblocks"
    )
  return intro


def extract_discharge_path_section(intro_text: str) -> str:
  discharge_header = require_unique_match(
    DISCHARGE_PATH_HEADER_PATTERN,
    intro_text,
    "Discharge Path header",
  )
  registry_header = require_unique_match(
    OBLIGATION_REGISTRY_HEADER_PATTERN,
    intro_text,
    "Obligation Registry header",
  )
  if registry_header.start() <= discharge_header.end():
    raise SemanticBridgeReadinessSummaryError(
      "SemanticBridgeReadiness Discharge Path section has an invalid boundary ordering"
    )
  discharge_section = intro_text[discharge_header.end() : registry_header.start()]
  if not discharge_section.strip():
    raise SemanticBridgeReadinessSummaryError(
      "SemanticBridgeReadiness Discharge Path section is empty"
    )
  return discharge_section


def extract_namespace_body(text: str) -> str:
  namespace_blocks = extract_namespace_blocks(text)
  _, _, primary_namespace_body = namespace_blocks[0]
  if not namespace_block_has_tracked_summary(primary_namespace_body):
    raise SemanticBridgeReadinessSummaryError(
      "primary SemanticBridgeReadiness namespace block is missing tracked summary content"
    )
  duplicate_summary_blocks = [
    match.group(0).strip()
    for match, _, namespace_body in namespace_blocks[1:]
    if namespace_block_has_tracked_summary(namespace_body)
  ]
  if duplicate_summary_blocks:
    raise SemanticBridgeReadinessSummaryError(
      "found multiple SemanticBridgeReadiness namespace blocks with tracked summary content"
    )
  return primary_namespace_body


def parse_operation_list(raw_ops: str) -> list[str]:
  stripped = raw_ops.strip()
  if not stripped:
    raise SemanticBridgeReadinessSummaryError("Link 1 operation list is empty")
  if stripped.lower() == "none":
    return []

  operations = [item.strip().strip("`") for item in raw_ops.replace("\n", " ").split(",")]
  if any(not item for item in operations):
    raise SemanticBridgeReadinessSummaryError("Link 1 operation list contains an empty entry")
  if any(item.lower() == "none" for item in operations):
    raise SemanticBridgeReadinessSummaryError(
      "Link 1 operation list mixes the none sentinel with named operations"
    )
  return operations


def require_count(
  actual: int,
  expected: int,
  description: str,
) -> None:
  if actual != expected:
    raise SemanticBridgeReadinessSummaryError(
      f"{description} drift: expected {expected}, found {actual}"
    )


def validate_summary(text: str, summary: dict[str, Any]) -> None:
  intro_text = extract_intro_section(text)
  discharge_path_text = extract_discharge_path_section(intro_text)
  namespace_body = extract_namespace_body(text)

  if DISCHARGE_PATH_PREFIX not in discharge_path_text:
    raise SemanticBridgeReadinessSummaryError(
      "discharge-path upstream status drift: "
      f"expected `{DISCHARGE_PATH_PREFIX}`"
    )

  total_comment = require_unique_match(TOTAL_COMMENT_RE, namespace_body, "obligation summary comment")
  require_count(int(total_comment.group("count")), summary["total"], "obligation summary comment count")

  link1_comment = require_unique_match(LINK1_COMMENT_RE, namespace_body, "Link 1 summary comment")
  require_count(int(link1_comment.group("count")), summary["link1_count"], "Link 1 summary comment count")
  require_count(int(link1_comment.group("total")), summary["total"], "Link 1 summary total")

  link1_list = require_unique_match(LINK1_LIST_RE, namespace_body, "Link 1 operation list")
  require_count(int(link1_list.group("count")), summary["link1_count"], "Link 1 operation list count")
  actual_operations = parse_operation_list(link1_list.group("ops"))
  if not summary["link1_operations"] and actual_operations:
    raise SemanticBridgeReadinessSummaryError(
      "Link 1 operation list drift: expected no operations; found "
      + ", ".join(actual_operations)
    )
  if len(actual_operations) != len(set(actual_operations)):
    raise SemanticBridgeReadinessSummaryError(
      "Link 1 operation list contains duplicate operations"
    )
  if set(actual_operations) != set(summary["link1_operations"]):
    raise SemanticBridgeReadinessSummaryError(
      "Link 1 operation list drift: expected "
      + ", ".join(summary["link1_operations"])
      + "; found "
      + ", ".join(actual_operations)
    )

  assumed_comment = require_unique_match(ASSUMED_COMMENT_RE, namespace_body, "assumed-count comment")
  require_count(int(assumed_comment.group("count")), summary["assumed_count"], "assumed-count comment")

  migrated_comment = require_unique_match(MIGRATED_COMMENT_RE, namespace_body, "macro-migrated comment")
  require_count(
    int(migrated_comment.group("count")),
    summary["macro_migrated_count"],
    "macro-migrated comment count",
  )
  require_count(
    int(migrated_comment.group("total")),
    summary["total"],
    "macro-migrated comment total",
  )

  pending_comment = require_unique_match(PENDING_COMMENT_RE, namespace_body, "macro-pending comment")
  require_count(
    int(pending_comment.group("count")),
    summary["macro_pending_count"],
    "macro-pending comment",
  )

  theorem_expectations = {
    "obligation_count": summary["total"],
    "link1_proven_count": summary["link1_count"],
    "assumed_count": summary["assumed_count"],
    "macro_migrated_count": summary["macro_migrated_count"],
    "macro_pending_count": summary["macro_pending_count"],
  }
  for theorem_name, expected in theorem_expectations.items():
    match = require_unique_match(
      THEOREM_PATTERNS[theorem_name],
      namespace_body,
      f"{theorem_name} theorem",
    )
    require_count(int(match.group("count")), expected, f"{theorem_name} theorem")


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate the proof-facing summary in SemanticBridgeReadiness.lean"
  )
  parser.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
  parser.add_argument("--readiness", type=pathlib.Path, default=READINESS_PATH)
  args = parser.parse_args()
  config_path = args.config.resolve()
  readiness_path = args.readiness.resolve()

  try:
    summary = derive_summary(load_config(config_path))
  except SemanticBridgeReadinessSyncError as exc:
    raise SemanticBridgeReadinessSummaryError(str(exc)) from exc
  text = read_readiness_text(readiness_path)
  validate_summary(text, summary)

  print("semantic-bridge-readiness-summary check: OK")
  print(
    "counts: "
    f"total={summary['total']} "
    f"link1={summary['link1_count']} "
    f"assumed={summary['assumed_count']} "
    f"macro_migrated={summary['macro_migrated_count']} "
    f"macro_pending={summary['macro_pending_count']}"
  )
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except SemanticBridgeReadinessSummaryError as e:
    print(f"semantic-bridge-readiness-summary check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
