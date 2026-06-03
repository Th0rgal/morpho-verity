#!/usr/bin/env python3

from __future__ import annotations

import pathlib
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import check_morpho_midnight_artifact_boundary as check


class CheckMorphoMidnightArtifactBoundaryTest(unittest.TestCase):
    def test_sha256_distinguishes_content(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            a = root / "a.bin"
            b = root / "b.bin"
            a.write_bytes(b"focused")
            b.write_bytes(b"complete")
            self.assertNotEqual(check.sha256(a), check.sha256(b))

    def test_require_nonempty_rejects_missing(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            missing = pathlib.Path(tmp) / "missing.bin"
            with self.assertRaises(check.MidnightArtifactBoundaryError):
                check.require_nonempty(missing, "missing test artifact")

    def test_parse_manifest_rejects_duplicates(self) -> None:
        with self.assertRaises(check.MidnightArtifactBoundaryError):
            check.parse_manifest("input_digest=a\ninput_digest=b\n")

    def test_repo_boundary_is_clean(self) -> None:
        self.assertEqual(check.main(), 0)


if __name__ == "__main__":
    unittest.main()
