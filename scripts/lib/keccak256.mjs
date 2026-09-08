// Keccak-256 (Ethereum padding 0x01, not FIPS SHA3 padding 0x06).
// Dependency-free, BigInt lanes; used only at generation time.
const MASK = (1n << 64n) - 1n
const RC = [
  0x0000000000000001n, 0x0000000000008082n, 0x800000000000808an, 0x8000000080008000n,
  0x000000000000808bn, 0x0000000080000001n, 0x8000000080008081n, 0x8000000000008009n,
  0x000000000000008an, 0x0000000000000088n, 0x0000000080008009n, 0x000000008000000an,
  0x000000008000808bn, 0x800000000000008bn, 0x8000000000008089n, 0x8000000000008003n,
  0x8000000000008002n, 0x8000000000000080n, 0x000000000000800an, 0x800000008000000an,
  0x8000000080008081n, 0x8000000000008080n, 0x0000000080000001n, 0x8000000080008008n,
]
const ROT = [0, 1, 62, 28, 27, 36, 44, 6, 55, 20, 3, 10, 43, 25, 39, 41, 45, 15, 21, 8, 18, 2, 61, 56, 14]
const rot = (v, n) => ((v << BigInt(n)) | (v >> BigInt(64 - n))) & MASK
function permute(a) {
  for (const rc of RC) {
    const c = Array.from({ length: 5 }, (_, x) => a[x] ^ a[x + 5] ^ a[x + 10] ^ a[x + 15] ^ a[x + 20])
    const d = c.map((_, x) => c[(x + 4) % 5] ^ rot(c[(x + 1) % 5], 1))
    for (let i = 0; i < 25; i++) a[i] ^= d[i % 5]
    const b = Array(25).fill(0n)
    for (let x = 0; x < 5; x++) for (let y = 0; y < 5; y++) b[y + 5 * ((2 * x + 3 * y) % 5)] = rot(a[x + 5 * y], ROT[x + 5 * y])
    for (let x = 0; x < 5; x++) for (let y = 0; y < 5; y++) a[x + 5 * y] = b[x + 5 * y] ^ ((~b[(x + 1) % 5 + 5 * y]) & b[(x + 2) % 5 + 5 * y])
    a[0] ^= rc
  }
}
export function keccak256(input) {
  const bytes = Buffer.from(input)
  const padded = Buffer.alloc(Math.ceil((bytes.length + 1) / 136) * 136)
  bytes.copy(padded); padded[bytes.length] = 1; padded[padded.length - 1] |= 0x80
  const state = Array(25).fill(0n)
  for (let block = 0; block < padded.length; block += 136) {
    for (let lane = 0; lane < 17; lane++) state[lane] ^= padded.readBigUInt64LE(block + lane * 8)
    permute(state)
  }
  const output = Buffer.alloc(32)
  for (let i = 0; i < 4; i++) output.writeBigUInt64LE(state[i], i * 8)
  return output.toString('hex')
}
