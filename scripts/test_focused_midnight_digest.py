#!/usr/bin/env python3

from __future__ import annotations

import hashlib
import pathlib
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import focused_midnight_digest as focused


class FocusedMidnightDigestTest(unittest.TestCase):
    def test_digest_uses_repo_relative_paths(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            for index, rel in enumerate(focused.FOCUSED_INPUTS):
                path = root / rel
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(f"input {index}\n", encoding="utf-8")

            digest_input = "".join(
                f"{hashlib.sha256((root / rel).read_bytes()).hexdigest()}  {rel}\n"
                for rel in focused.FOCUSED_INPUTS
            )
            expected = hashlib.sha256(digest_input.encode("utf-8")).hexdigest()

            self.assertEqual(focused.compute_focused_input_digest(root), expected)
            self.assertNotIn(str(root), digest_input)

    def test_main_rejects_missing_input(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            self.assertEqual(focused.main_with_root(pathlib.Path(tmp)), 2)


if __name__ == "__main__":
    unittest.main()
