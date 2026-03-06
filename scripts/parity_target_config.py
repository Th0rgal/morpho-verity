#!/usr/bin/env python3
"""Shared helpers for config/parity-target.json parsing."""

from __future__ import annotations

from typing import Any


ALLOWED_YUL_IDENTITY_GATE_MODES = {"unsupported-manifest", "exact"}


def parse_yul_identity_gate_mode(
    target: dict[str, Any],
    *,
    missing_message: str,
    invalid_message_prefix: str,
    invalid_message_suffix: str = "",
) -> str:
  yul_identity = target.get("yulIdentity")
  if not isinstance(yul_identity, dict):
    raise RuntimeError(missing_message)
  gate_mode = yul_identity.get("gateMode")
  if not isinstance(gate_mode, str) or gate_mode not in ALLOWED_YUL_IDENTITY_GATE_MODES:
    allowed = ", ".join(sorted(ALLOWED_YUL_IDENTITY_GATE_MODES))
    raise RuntimeError(f"{invalid_message_prefix} (expected one of: {allowed}){invalid_message_suffix}")
  return gate_mode
