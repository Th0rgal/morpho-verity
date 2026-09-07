import unittest
from compile_midnight_yul import prepare

BASE = '''object "Midnight" {
    code {
        mstore(64, 128)
        mstore(384, 0)
        datacopy(0, dataoffset("runtime"), datasize("runtime"))
        return(0, datasize("runtime"))
    }
}'''

class BackendPolicyTest(unittest.TestCase):
    def test_scratch_and_deployment_copy(self):
        text, size, count = prepare(BASE)
        self.assertEqual((size, count), (416, 1))
        self.assertIn('memoryguard(416)', text)
        self.assertIn('datacopy(mload(64),', text)
        self.assertIn('return(mload(64),', text)
        self.assertEqual(prepare(BASE), prepare(BASE))

    def test_scratch_derived_from_actual_words(self):
        self.assertEqual(prepare(BASE.replace('384', '512'))[1], 544)

    def test_unknown_mechanisms_fail_closed(self):
        for op in ['mstore8(0, 1)', 'mcopy(0, 1, 2)', 'codecopy(0, 1, 2)']:
            with self.assertRaisesRegex(ValueError, 'unreviewed'):
                prepare(BASE.replace('mstore(384, 0)', op))

    def test_missing_initializer_copy_and_double_lowering_rejected(self):
        for text in [BASE.replace('mstore(64, 128)', ''), BASE.replace('return(0, datasize', 'return(1, datasize'), prepare(BASE)[0]]:
            with self.assertRaises(ValueError):
                prepare(text)

if __name__ == '__main__':
    unittest.main()
