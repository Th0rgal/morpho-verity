#!/usr/bin/env python3
"""Source-envelope regressions using temporary Git repos and the pinned real solc.

Run after provisioning the importer compiler cache:
    python3 scripts/test_midnight_source_pin.py
No files in the upstream Solidity submodule are modified.
"""
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
NODE_PROBE = """import {compilePinnedSource} from './scripts/lib/midnight-source.mjs';
let text=''; for await (const chunk of process.stdin) text += chunk;
try {
  const result=compilePinnedSource(process.cwd(),JSON.parse(text));
  console.log(JSON.stringify({accepted:true,sources:Object.keys(result.input.sources)}));
} catch (error) {
  console.log(JSON.stringify({accepted:false,error:error.message}));
}
"""


class SourcePinTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix='midnight-source-pin-test-')
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.git('init', '-q')
        (self.root / 'src').mkdir()
        self.original = self.root / 'src/Original.sol'
        self.original.write_text('pragma solidity ^0.8.0; contract Original { uint public x; }\n')
        self.git('add', 'src/Original.sol')
        self.git('-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid',
                 '-c', 'commit.gpgsign=false', 'commit', '-qm', 'source pin fixture')
        self.config = json.loads((ROOT / 'config/midnight-admin-import.json').read_text())
        self.config.update(sourceRoot=str(self.root), midnightCommit=self.git('rev-parse', 'HEAD'))

    def git(self, *args):
        return subprocess.check_output(
            ['git', '-c', 'core.hooksPath=/dev/null', '-C', str(self.root), *args],
            text=True, stderr=subprocess.STDOUT).strip()

    def probe(self):
        result = subprocess.check_output(['node', '--input-type=module', '-e', NODE_PROBE],
                                         cwd=ROOT, input=json.dumps(self.config), text=True)
        return json.loads(result)

    def test_clean_pinned_source_is_accepted(self):
        self.assertEqual(self.probe(), {'accepted': True, 'sources': ['src/Original.sol']})

    def test_ignored_extra_solidity_is_rejected(self):
        (self.root / '.git/info/exclude').write_text('src/Ignored.sol\n')
        (self.root / 'src/Ignored.sol').write_text('pragma solidity ^0.8.0; contract Ignored {}\n')
        self.assertEqual(self.git('status', '--porcelain', '--untracked-files=all', '--', 'src'), '')
        result = self.probe()
        self.assertFalse(result['accepted'])
        self.assertIn('source set differs from pinned tree', result['error'])

    def check_hidden_source_edit(self, index_flag):
        self.git('update-index', index_flag, 'src/Original.sol')
        self.original.write_text('pragma solidity ^0.8.0; contract Original { uint public changed; }\n')
        self.assertEqual(self.git('status', '--porcelain', '--untracked-files=all', '--', 'src'), '')
        result = self.probe()
        self.assertFalse(result['accepted'])
        self.assertIn('source bytes differ from pinned tree: src/Original.sol', result['error'])

    def test_assume_unchanged_does_not_bypass_pin(self):
        self.check_hidden_source_edit('--assume-unchanged')

    def test_skip_worktree_does_not_bypass_pin(self):
        self.check_hidden_source_edit('--skip-worktree')


if __name__ == '__main__':
    unittest.main()
