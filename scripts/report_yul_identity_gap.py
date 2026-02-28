#!/usr/bin/env python3
"""Generate machine-readable Yul parity report for Morpho (Solidity vs Verity)."""

from __future__ import annotations

import argparse
import difflib
import hashlib
import json
import os
import pathlib
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
PARITY_TARGET = ROOT / "config" / "parity-target.json"
VERITY_YUL = ROOT / "compiler" / "yul" / "Morpho.yul"
SOLIDITY_ARTIFACT = ROOT / "morpho-blue" / "out" / "Morpho.sol" / "Morpho.json"
RUN_WITH_TIMEOUT = ROOT / "scripts" / "run_with_timeout.sh"
DEFAULT_OUT_DIR = ROOT / "out" / "parity-target"
DEFAULT_UNSUPPORTED_MANIFEST = ROOT / "config" / "yul-identity-unsupported.json"


@dataclass(frozen=True)
class YulToken:
  value: str
  line: int
  column: int


@dataclass(frozen=True)
class FunctionAstDigest:
  name: str
  ordinal: int
  key: str
  digest: str
  body_digest: str
  tokens: tuple[YulToken, ...]


def sha256_text(text: str) -> str:
  return hashlib.sha256(text.encode("utf-8")).hexdigest()


def run(cmd: list[str], *, cwd: pathlib.Path, env: dict[str, str] | None = None) -> None:
  merged_env = os.environ.copy()
  if env is not None:
    merged_env.update(env)
  subprocess.check_call(cmd, cwd=str(cwd), env=merged_env)


def run_with_timeout(
    timeout_env: str,
    default_timeout_sec: int,
    description: str,
    cmd: list[str],
    *,
    cwd: pathlib.Path,
    env: dict[str, str] | None = None,
) -> None:
  run(
    [
      str(RUN_WITH_TIMEOUT),
      timeout_env,
      str(default_timeout_sec),
      description,
      "--",
      *cmd,
    ],
    cwd=cwd,
    env=env,
  )


def read_json(path: pathlib.Path) -> dict[str, Any]:
  with path.open("r", encoding="utf-8") as f:
    return json.load(f)


def read_text(path: pathlib.Path) -> str:
  with path.open("r", encoding="utf-8") as f:
    return f.read()


def write_text(path: pathlib.Path, content: str) -> None:
  path.parent.mkdir(parents=True, exist_ok=True)
  with path.open("w", encoding="utf-8") as f:
    f.write(content)


def display_path(path: pathlib.Path) -> str:
  try:
    return str(path.relative_to(ROOT))
  except ValueError:
    return str(path)


def normalize_yul(text: str) -> str:
  # Drop comments and normalize trailing whitespace for stable comparisons.
  without_block_comments = re.sub(r"/\*.*?\*/", "", text, flags=re.DOTALL)
  without_line_comments = re.sub(r"//[^\n]*", "", without_block_comments)
  lines = [line.rstrip() for line in without_line_comments.splitlines() if line.strip()]
  return "\n".join(lines) + ("\n" if lines else "")


def tokenize_normalized_yul_with_spans(text: str) -> list[YulToken]:
  tokens: list[YulToken] = []
  i = 0
  n = len(text)
  line = 1
  column = 1

  def advance_char(ch: str) -> None:
    nonlocal line, column
    if ch == "\n":
      line += 1
      column = 1
    else:
      column += 1

  def advance_text(segment: str) -> None:
    for ch in segment:
      advance_char(ch)

  while i < n:
    ch = text[i]
    if ch.isspace():
      advance_char(ch)
      i += 1
      continue

    token_line = line
    token_column = column

    if ch in {'"', "'"}:
      quote = ch
      start = i
      i += 1
      escaped = False
      while i < n:
        c = text[i]
        if escaped:
          escaped = False
        elif c == "\\":
          escaped = True
        elif c == quote:
          i += 1
          break
        i += 1
      else:
        raise RuntimeError("Unterminated string literal while tokenizing normalized Yul.")
      value = text[start:i]
      tokens.append(YulToken(value=value, line=token_line, column=token_column))
      advance_text(value)
      continue

    if ch.isdigit():
      start = i
      i += 1
      while i < n and (text[i].isalnum() or text[i] == "_"):
        i += 1
      value = text[start:i]
      tokens.append(YulToken(value=value, line=token_line, column=token_column))
      advance_text(value)
      continue

    if ch.isalpha() or ch in {"_", "$"}:
      start = i
      i += 1
      while i < n and (text[i].isalnum() or text[i] in {"_", "$"}):
        i += 1
      value = text[start:i]
      tokens.append(YulToken(value=value, line=token_line, column=token_column))
      advance_text(value)
      continue

    if text.startswith("->", i):
      tokens.append(YulToken(value="->", line=token_line, column=token_column))
      i += 2
      advance_text("->")
      continue

    if text.startswith(":=", i):
      tokens.append(YulToken(value=":=", line=token_line, column=token_column))
      i += 2
      advance_text(":=")
      continue

    if ch in "{}()[],:;.":
      tokens.append(YulToken(value=ch, line=token_line, column=token_column))
      i += 1
      advance_char(ch)
      continue

    if ch in "+-*/%<>=!&|^~?":
      start = i
      i += 1
      while i < n and text[i] in "+-*/%<>=!&|^~?":
        i += 1
      value = text[start:i]
      tokens.append(YulToken(value=value, line=token_line, column=token_column))
      advance_text(value)
      continue

    raise RuntimeError(f"Unsupported character while tokenizing normalized Yul: {ch!r}")

  return tokens


def tokenize_normalized_yul(text: str) -> list[str]:
  return [tok.value for tok in tokenize_normalized_yul_with_spans(text)]


def render_struct_tokens(tokens: list[YulToken] | tuple[YulToken, ...]) -> str:
  return " ".join(tok.value for tok in tokens)


def find_matching_token(tokens: list[YulToken], start_index: int, open_tok: str, close_tok: str) -> int:
  depth = 1
  i = start_index
  while i < len(tokens):
    tok = tokens[i].value
    if tok == open_tok:
      depth += 1
    elif tok == close_tok:
      depth -= 1
      if depth == 0:
        return i
    i += 1
  raise RuntimeError(f"Unbalanced delimiters while parsing normalized Yul: {open_tok} .. {close_tok}")


def validate_balanced_delimiters(tokens: list[YulToken]) -> None:
  stack: list[str] = []
  opening = {"{": "}", "(": ")", "[": "]"}
  closing = {"}": "{", ")": "(", "]": "["}
  for tok in tokens:
    value = tok.value
    if value in opening:
      stack.append(value)
    elif value in closing:
      if not stack or stack[-1] != closing[value]:
        raise RuntimeError("Unbalanced delimiters while parsing normalized Yul.")
      stack.pop()
  if stack:
    raise RuntimeError("Unbalanced delimiters while parsing normalized Yul.")


def function_ast_digests(normalized_yul: str) -> dict[str, FunctionAstDigest]:
  tokens = tokenize_normalized_yul_with_spans(normalized_yul)
  validate_balanced_delimiters(tokens)
  n = len(tokens)
  i = 0
  counts: dict[str, int] = {}
  digests: dict[str, FunctionAstDigest] = {}

  while i < n:
    if tokens[i].value != "function":
      i += 1
      continue
    if i + 2 >= n:
      i += 1
      continue

    name = tokens[i + 1].value
    if not (name and (name[0].isalpha() or name[0] in {"_", "$"})):
      i += 1
      continue
    if tokens[i + 2].value != "(":
      i += 1
      continue

    params_end = find_matching_token(tokens, i + 3, "(", ")")
    cursor = params_end + 1
    if cursor < n and tokens[cursor].value == "->":
      cursor += 1
      while cursor < n and tokens[cursor].value != "{":
        cursor += 1
    if cursor >= n or tokens[cursor].value != "{":
      i += 1
      continue

    body_end = find_matching_token(tokens, cursor + 1, "{", "}")
    fn_tokens = tuple(tokens[i : body_end + 1])
    ordinal = counts.get(name, 0)
    counts[name] = ordinal + 1
    key = f"{name}#{ordinal}"
    digests[key] = FunctionAstDigest(
      name=name,
      ordinal=ordinal,
      key=key,
      digest=sha256_text(render_struct_tokens(fn_tokens)),
      body_digest=sha256_text(
        render_struct_tokens(tuple(tok for idx, tok in enumerate(fn_tokens) if idx != 1))
      ),
      tokens=fn_tokens,
    )
    i = body_end + 1

  return digests


def first_token_mismatch(solidity_tokens: tuple[YulToken, ...], verity_tokens: tuple[YulToken, ...]) -> dict[str, Any]:
  shared_len = min(len(solidity_tokens), len(verity_tokens))
  for idx in range(shared_len):
    if solidity_tokens[idx].value != verity_tokens[idx].value:
      return {
        "tokenIndex": idx,
        "solidityToken": solidity_tokens[idx].value,
        "verityToken": verity_tokens[idx].value,
        "solidityLine": solidity_tokens[idx].line,
        "solidityColumn": solidity_tokens[idx].column,
        "verityLine": verity_tokens[idx].line,
        "verityColumn": verity_tokens[idx].column,
      }

  if len(solidity_tokens) != len(verity_tokens):
    sol_extra = solidity_tokens[shared_len] if shared_len < len(solidity_tokens) else None
    ver_extra = verity_tokens[shared_len] if shared_len < len(verity_tokens) else None
    return {
      "tokenIndex": shared_len,
      "solidityToken": sol_extra.value if sol_extra is not None else None,
      "verityToken": ver_extra.value if ver_extra is not None else None,
      "solidityLine": sol_extra.line if sol_extra is not None else None,
      "solidityColumn": sol_extra.column if sol_extra is not None else None,
      "verityLine": ver_extra.line if ver_extra is not None else None,
      "verityColumn": ver_extra.column if ver_extra is not None else None,
    }

  return {
    "tokenIndex": -1,
    "solidityToken": None,
    "verityToken": None,
    "solidityLine": None,
    "solidityColumn": None,
    "verityLine": None,
    "verityColumn": None,
  }


def compare_function_hashes(
    solidity_digests: dict[str, FunctionAstDigest], verity_digests: dict[str, FunctionAstDigest]
) -> dict[str, list[str]]:
  only_solidity: list[str] = []
  only_verity: list[str] = []
  hash_mismatch: list[str] = []
  for key in sorted(set(solidity_digests) | set(verity_digests)):
    sol_digest = solidity_digests.get(key)
    ver_digest = verity_digests.get(key)
    if sol_digest is None:
      only_verity.append(key)
    elif ver_digest is None:
      only_solidity.append(key)
    elif sol_digest.digest != ver_digest.digest:
      hash_mismatch.append(key)
  return {
    "hashMismatch": hash_mismatch,
    "onlyInSolidity": only_solidity,
    "onlyInVerity": only_verity,
  }


def build_name_insensitive_pairs(
    solidity_digests: dict[str, FunctionAstDigest],
    verity_digests: dict[str, FunctionAstDigest],
    deltas: dict[str, list[str]],
) -> dict[str, Any]:
  only_sol = [solidity_digests[key] for key in deltas["onlyInSolidity"] if key in solidity_digests]
  only_ver = [verity_digests[key] for key in deltas["onlyInVerity"] if key in verity_digests]

  by_digest_sol: dict[str, list[FunctionAstDigest]] = {}
  by_digest_ver: dict[str, list[FunctionAstDigest]] = {}
  for fn in only_sol:
    by_digest_sol.setdefault(fn.body_digest, []).append(fn)
  for fn in only_ver:
    by_digest_ver.setdefault(fn.body_digest, []).append(fn)

  pair_matches: list[dict[str, Any]] = []
  ambiguous_groups: list[dict[str, Any]] = []
  for digest in sorted(set(by_digest_sol) & set(by_digest_ver)):
    sol_group = sorted(by_digest_sol[digest], key=lambda fn: fn.key)
    ver_group = sorted(by_digest_ver[digest], key=lambda fn: fn.key)
    if len(sol_group) == 1 and len(ver_group) == 1:
      pair_matches.append(
        {
          "bodyDigest": digest,
          "solidity": {"key": sol_group[0].key, "name": sol_group[0].name, "ordinal": sol_group[0].ordinal},
          "verity": {"key": ver_group[0].key, "name": ver_group[0].name, "ordinal": ver_group[0].ordinal},
        }
      )
    else:
      ambiguous_groups.append(
        {
          "bodyDigest": digest,
          "solidity": [{"key": fn.key, "name": fn.name, "ordinal": fn.ordinal} for fn in sol_group],
          "verity": [{"key": fn.key, "name": fn.name, "ordinal": fn.ordinal} for fn in ver_group],
        }
      )

  return {
    "comparator": "ast-function-body-digest-no-name-v1",
    "pairCount": len(pair_matches),
    "ambiguousGroupCount": len(ambiguous_groups),
    "pairs": pair_matches,
    "ambiguousGroups": ambiguous_groups,
  }


def function_family_for_key(key: str) -> str:
  base = key.split("#", 1)[0]
  base = re.sub(r"_[0-9]+$", "", base)
  if base.startswith("copy_literal_to_memory_"):
    return "copy_literal_to_memory"
  if base.startswith("abi_decode_"):
    tail = base[len("abi_decode_") :]
    head = tail.split("_", 1)[0] if tail else ""
    return f"abi_decode_{head}" if head else "abi_decode"
  if base.startswith("abi_encode_"):
    tail = base[len("abi_encode_") :]
    head = tail.split("_", 1)[0] if tail else ""
    return f"abi_encode_{head}" if head else "abi_encode"
  if base.startswith("checked_"):
    parts = base.split("_")
    if len(parts) >= 2:
      return "_".join(parts[:2])
  if base.startswith("finalize_allocation"):
    return "finalize_allocation"
  if base.startswith("update_storage_value_"):
    return "update_storage_value"
  if base.startswith("array_allocation_size_"):
    return "array_allocation_size"
  if base.startswith("fun_"):
    return "fun"
  return base


def _family_entries(keys: list[str]) -> list[dict[str, Any]]:
  grouped: dict[str, list[str]] = {}
  for key in sorted(keys):
    grouped.setdefault(function_family_for_key(key), []).append(key)
  entries = [
    {
      "family": family,
      "count": len(group_keys),
      "keys": group_keys,
    }
    for family, group_keys in grouped.items()
  ]
  entries.sort(key=lambda item: (-item["count"], item["family"]))
  return entries


def build_function_family_summary(deltas: dict[str, list[str]]) -> dict[str, Any]:
  only_sol = _family_entries(deltas["onlyInSolidity"])
  only_ver = _family_entries(deltas["onlyInVerity"])
  return {
    "version": "function-family-v1",
    "onlyInSolidity": only_sol,
    "onlyInVerity": only_ver,
    "priorityOnlyInSolidityFamilies": [
      {"family": item["family"], "count": item["count"]} for item in only_sol
    ],
  }


def load_unsupported_manifest(path: pathlib.Path) -> dict[str, Any]:
  data = read_json(path)
  if not isinstance(data, dict):
    raise RuntimeError(f"Unsupported manifest must be a JSON object: {path}")
  parity_target = data.get("parityTarget")
  if not isinstance(parity_target, str) or not parity_target.strip():
    raise RuntimeError(
      f"Unsupported manifest key `parityTarget` must be a non-empty string: {path}"
    )
  for key in ("allowedHashMismatchKeys", "allowedOnlyInSolidityKeys", "allowedOnlyInVerityKeys"):
    value = data.get(key)
    if not isinstance(value, list) or not all(isinstance(x, str) for x in value):
      raise RuntimeError(f"Unsupported manifest key `{key}` must be a list of strings: {path}")
  return data


def evaluate_unsupported_manifest(
    deltas: dict[str, list[str]], manifest: dict[str, Any], active_parity_target: str
) -> dict[str, Any]:
  expected_target = manifest.get("parityTarget")
  target_ok = expected_target == active_parity_target
  expected_hash = sorted(manifest.get("allowedHashMismatchKeys", []))
  expected_sol = sorted(manifest.get("allowedOnlyInSolidityKeys", []))
  expected_ver = sorted(manifest.get("allowedOnlyInVerityKeys", []))
  actual_hash = sorted(deltas["hashMismatch"])
  actual_sol = sorted(deltas["onlyInSolidity"])
  actual_ver = sorted(deltas["onlyInVerity"])

  unexpected_hash = sorted(set(actual_hash) - set(expected_hash))
  unexpected_sol = sorted(set(actual_sol) - set(expected_sol))
  unexpected_ver = sorted(set(actual_ver) - set(expected_ver))
  missing_hash = sorted(set(expected_hash) - set(actual_hash))
  missing_sol = sorted(set(expected_sol) - set(actual_sol))
  missing_ver = sorted(set(expected_ver) - set(actual_ver))

  return {
    "ok": target_ok and not (
      unexpected_hash or unexpected_sol or unexpected_ver or missing_hash or missing_sol or missing_ver
    ),
    "parityTarget": {
      "expected": expected_target,
      "actual": active_parity_target,
      "ok": target_ok,
    },
    "expected": {
      "hashMismatch": expected_hash,
      "onlyInSolidity": expected_sol,
      "onlyInVerity": expected_ver,
    },
    "actual": {
      "hashMismatch": actual_hash,
      "onlyInSolidity": actual_sol,
      "onlyInVerity": actual_ver,
    },
    "unexpected": {
      "hashMismatch": unexpected_hash,
      "onlyInSolidity": unexpected_sol,
      "onlyInVerity": unexpected_ver,
    },
    "missingExpected": {
      "hashMismatch": missing_hash,
      "onlyInSolidity": missing_sol,
      "onlyInVerity": missing_ver,
    },
  }


def extract_solidity_ir_optimized() -> str:
  data = read_json(SOLIDITY_ARTIFACT)
  ir = data.get("irOptimized")
  if not isinstance(ir, str) or not ir.strip():
    raise RuntimeError(
      f"Missing `irOptimized` in {SOLIDITY_ARTIFACT}. "
      "Run forge with `--extra-output irOptimized`."
    )
  return ir


def compile_solidity_ir() -> None:
  run_with_timeout(
    "MORPHO_SOLIDITY_IR_BUILD_TIMEOUT_SEC",
    1200,
    "Build Solidity IR for Yul identity report",
    [
      "forge",
      "build",
      "--force",
      "--skip",
      "test",
      "script",
      "--contracts",
      "src/Morpho.sol",
      "--extra-output",
      "irOptimized",
      "-q",
    ],
    cwd=ROOT / "morpho-blue",
  )


def compile_verity_yul() -> None:
  # The Yul identity report only consumes Morpho.yul; skip solc bytecode generation.
  run_with_timeout(
    "MORPHO_VERITY_PREP_TIMEOUT_SEC",
    900,
    "Prepare Verity artifact for Yul identity report",
    ["./scripts/prepare_verity_morpho_artifact.sh"],
    cwd=ROOT,
    env={"MORPHO_VERITY_SKIP_SOLC": "1"},
  )


def build_report(verity_yul: str, solc_ir: str, max_diff_lines: int) -> tuple[dict[str, Any], str]:
  normalized_verity = normalize_yul(verity_yul)
  normalized_solc = normalize_yul(solc_ir)

  solidity_function_digests = function_ast_digests(normalized_solc)
  verity_function_digests = function_ast_digests(normalized_verity)
  function_deltas = compare_function_hashes(solidity_function_digests, verity_function_digests)
  name_insensitive_pairs = build_name_insensitive_pairs(
    solidity_function_digests, verity_function_digests, function_deltas
  )
  family_summary = build_function_family_summary(function_deltas)

  solidity_tokens = tokenize_normalized_yul_with_spans(normalized_solc)
  verity_tokens = tokenize_normalized_yul_with_spans(normalized_verity)
  validate_balanced_delimiters(solidity_tokens)
  validate_balanced_delimiters(verity_tokens)

  ast_equal = [tok.value for tok in solidity_tokens] == [tok.value for tok in verity_tokens]
  raw_equal = verity_yul == solc_ir
  normalized_equal = normalized_verity == normalized_solc

  diff_lines: list[str] = []
  if not normalized_equal:
    diff_lines = list(
      difflib.unified_diff(
        normalized_solc.splitlines(),
        normalized_verity.splitlines(),
        fromfile="solidity/Morpho.irOptimized.normalized.yul",
        tofile="verity/Morpho.normalized.yul",
        lineterm="",
      )
    )

  mismatch_details: list[dict[str, Any]] = []
  for key in function_deltas["hashMismatch"]:
    sol_fn = solidity_function_digests[key]
    ver_fn = verity_function_digests[key]
    mismatch = first_token_mismatch(sol_fn.tokens, ver_fn.tokens)
    mismatch_details.append(
      {
        "key": key,
        "name": sol_fn.name,
        "ordinal": sol_fn.ordinal,
        "firstMismatch": mismatch,
        "solidityDigest": sol_fn.digest,
        "verityDigest": ver_fn.digest,
      }
    )

  report = {
    "status": "match" if ast_equal else "mismatch",
    "astComparatorVersion": "yul-struct-v1",
    "rawEqual": raw_equal,
    "normalizedEqual": normalized_equal,
    "astEqual": ast_equal,
    "raw": {
      "solidity": {"bytes": len(solc_ir.encode("utf-8")), "sha256": sha256_text(solc_ir)},
      "verity": {"bytes": len(verity_yul.encode("utf-8")), "sha256": sha256_text(verity_yul)},
    },
    "normalized": {
      "solidity": {"bytes": len(normalized_solc.encode("utf-8")), "sha256": sha256_text(normalized_solc)},
      "verity": {"bytes": len(normalized_verity.encode("utf-8")), "sha256": sha256_text(normalized_verity)},
    },
    "ast": {
      "solidity": {
        "tokenCount": len(solidity_tokens),
        "sha256": sha256_text(render_struct_tokens(solidity_tokens)),
      },
      "verity": {
        "tokenCount": len(verity_tokens),
        "sha256": sha256_text(render_struct_tokens(verity_tokens)),
      },
      "firstMismatch": first_token_mismatch(tuple(solidity_tokens), tuple(verity_tokens)),
    },
    "functionBlocks": {
      "comparator": "ast-function-digest",
      "solidityCount": len(solidity_function_digests),
      "verityCount": len(verity_function_digests),
      "hashMismatch": function_deltas["hashMismatch"],
      "onlyInSolidity": function_deltas["onlyInSolidity"],
      "onlyInVerity": function_deltas["onlyInVerity"],
      "mismatchDetails": mismatch_details,
      "nameInsensitivePairs": name_insensitive_pairs,
      "familySummary": family_summary,
    },
    "diff": {"lineCount": len(diff_lines), "truncated": len(diff_lines) > max_diff_lines},
  }

  if len(diff_lines) > max_diff_lines:
    diff_lines = diff_lines[:max_diff_lines]

  return report, "\n".join(diff_lines) + ("\n" if diff_lines else "")


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument(
    "--out-dir",
    default=str(DEFAULT_OUT_DIR),
    help="Output directory for generated fixtures/report (default: out/parity-target).",
  )
  parser.add_argument(
    "--max-diff-lines",
    type=int,
    default=4000,
    help="Maximum number of diff lines stored in the artifact.",
  )
  parser.add_argument(
    "--strict",
    action="store_true",
    help="Exit non-zero when structural AST outputs differ.",
  )
  parser.add_argument(
    "--unsupported-manifest",
    default=str(DEFAULT_UNSUPPORTED_MANIFEST),
    help=(
      "JSON manifest of currently allowed function-level identity gaps "
      "(default: config/yul-identity-unsupported.json)."
    ),
  )
  parser.add_argument(
    "--enforce-unsupported-manifest",
    action="store_true",
    help="Exit non-zero when function-level mismatches diverge from the unsupported manifest.",
  )
  parser.add_argument(
    "--skip-build",
    action="store_true",
    help="Do not rebuild Solidity/Verity artifacts before generating the report.",
  )
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  out_dir = pathlib.Path(args.out_dir).resolve()
  unsupported_manifest_path = pathlib.Path(args.unsupported_manifest).resolve()
  solc_dir = out_dir / "solidity"
  verity_dir = out_dir / "verity"
  out_dir.mkdir(parents=True, exist_ok=True)
  solc_dir.mkdir(parents=True, exist_ok=True)
  verity_dir.mkdir(parents=True, exist_ok=True)

  target = read_json(PARITY_TARGET)
  if not args.skip_build:
    compile_verity_yul()
    compile_solidity_ir()

  verity_yul = read_text(VERITY_YUL)
  solc_ir = extract_solidity_ir_optimized()

  write_text(solc_dir / "Morpho.irOptimized.yul", solc_ir)
  write_text(verity_dir / "Morpho.yul", verity_yul)
  write_text(solc_dir / "Morpho.irOptimized.normalized.yul", normalize_yul(solc_ir))
  write_text(verity_dir / "Morpho.normalized.yul", normalize_yul(verity_yul))

  report, diff_text = build_report(verity_yul, solc_ir, args.max_diff_lines)
  report["parityTarget"] = target["id"]
  report["unsupportedManifest"] = {"path": display_path(unsupported_manifest_path), "found": False, "check": None}
  if unsupported_manifest_path.exists():
    manifest = load_unsupported_manifest(unsupported_manifest_path)
    report["unsupportedManifest"]["found"] = True
    report["unsupportedManifest"]["check"] = evaluate_unsupported_manifest(
      report["functionBlocks"], manifest, target["id"]
    )
  report["paths"] = {
    "solidityRaw": display_path(solc_dir / "Morpho.irOptimized.yul"),
    "verityRaw": display_path(verity_dir / "Morpho.yul"),
    "diff": display_path(out_dir / "normalized.diff"),
  }

  write_text(out_dir / "normalized.diff", diff_text)
  write_text(out_dir / "report.json", json.dumps(report, indent=2) + "\n")
  shutil.copy2(PARITY_TARGET, out_dir / "parity-target.json")

  print(f"parity-target: {target['id']}")
  print(f"status: {report['status']}")
  print(f"rawEqual: {report['rawEqual']}")
  print(f"normalizedEqual: {report['normalizedEqual']}")
  print(f"astEqual: {report['astEqual']}")
  print(f"report: {display_path(out_dir / 'report.json')}")
  print(f"diff: {display_path(out_dir / 'normalized.diff')}")
  if report["unsupportedManifest"]["found"]:
    print(f"unsupportedManifest: {display_path(unsupported_manifest_path)}")
    print(f"unsupportedManifestOk: {report['unsupportedManifest']['check']['ok']}")
    print(f"unsupportedManifestTargetOk: {report['unsupportedManifest']['check']['parityTarget']['ok']}")
  else:
    print(f"unsupportedManifest: missing ({display_path(unsupported_manifest_path)})")

  if args.strict and not report["astEqual"]:
    print("yul-identity check failed in strict mode", file=sys.stderr)
    return 1
  if args.enforce_unsupported_manifest:
    if not report["unsupportedManifest"]["found"]:
      print("yul-identity check failed: unsupported manifest file is missing", file=sys.stderr)
      return 1
    if not report["unsupportedManifest"]["check"]["ok"]:
      print("yul-identity check failed: unsupported manifest drift detected", file=sys.stderr)
      return 1
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
