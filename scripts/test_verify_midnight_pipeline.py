import unittest
from verify_midnight_pipeline import parse_suite

class EvidenceTest(unittest.TestCase):
    def test_completed_suite(self):
        row = "Ran 25 test suites in 1s: 373 tests passed, 0 failed, 0 skipped (373 total tests)"
        self.assertEqual(parse_suite(row)["total"], 373)

    def test_missing_partial_failed_skipped_and_mismatched_counts(self):
        for text in ["", "Build completed successfully.",
            "Ran 25 test suites in 1s: 372 tests passed, 1 failed, 0 skipped (373 total tests)",
            "Ran 25 test suites in 1s: 372 tests passed, 0 failed, 1 skipped (373 total tests)",
            "Ran 25 test suites in 1s: 373 tests passed, 0 failed, 0 skipped (374 total tests)",
            "Ran 1 test suites in 1s: 1 tests passed, 0 failed, 0 skipped (1 total tests)"]:
            with self.assertRaises(ValueError):
                parse_suite(text)

    def test_stale_success_is_cleared_before_work_and_failure_is_recorded(self):
        import json
        import subprocess
        import tempfile
        from pathlib import Path
        from unittest.mock import patch
        import verify_midnight_pipeline as verifier
        with tempfile.TemporaryDirectory() as directory:
            evidence = Path(directory)/'evidence.json'
            evidence.write_text('{"complete":true}')
            def fail_command(*args, **kwargs):
                checkpoint = json.loads(evidence.read_text())
                self.assertFalse(checkpoint['complete'])
                self.assertIsNone(checkpoint['commands'][0]['returncode'])
                return subprocess.CompletedProcess(args[0], 1)
            with patch('sys.argv', ['verify', '--out-dir', directory]), \
                 patch.object(verifier.subprocess, 'check_output', return_value='unit-test-revision'), \
                 patch.object(verifier.subprocess, 'run', side_effect=fail_command):
                with self.assertRaises(RuntimeError):
                    verifier.main()
            result=json.loads(evidence.read_text())
            self.assertFalse(result['complete'])
            self.assertEqual(result['commands'][0]['returncode'], 1)
            self.assertTrue((Path(directory)/result['commands'][0]['log']).exists())
            self.assertIn('error',result)

if __name__ == "__main__":
    unittest.main()
