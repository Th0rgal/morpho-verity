#!/usr/bin/env bash
set -euo pipefail

SOLC_VERSION="${1:-0.8.28}"

if ! command -v solc-select >/dev/null 2>&1; then
  pip3 install solc-select
fi

if ! solc-select versions | grep -q "${SOLC_VERSION}"; then
  solc-select install "${SOLC_VERSION}"
fi

solc-select use "${SOLC_VERSION}"
