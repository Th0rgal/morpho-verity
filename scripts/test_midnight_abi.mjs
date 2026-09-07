#!/usr/bin/env node
import assert from 'node:assert/strict'
import { readFileSync, writeFileSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { resolve, dirname } from 'node:path'
import { fileURLToPath } from 'node:url'
import { spawnSync } from 'node:child_process'
import * as abi from './lib/midnight-abi.mjs'
import { MidnightSource, compilePinnedSource } from './lib/midnight-source.mjs'
const root = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const L = n => `(.literal ${n})`
const scalar = (bits = 256, extra = {}) => ({ kind: 'scalar', abi: `uint${bits}`, bits, signed: false, ...extra })
const bytes = { kind: 'bytes', abi: 'bytes' }
const U = scalar(), A = scalar(160, { abi: 'address' })
const MASK = (1n << 256n) - 1n
let id = 0
function context() {
  const statements = []
  const ctx = { statements, emit(s) { statements.push(s) }, fresh(h) { return `${h}_${id++}` }, fail(s) { throw Error(s) } }
  ctx.bind = (e, h) => { const n = ctx.fresh(h); ctx.emit(`.letVar ${JSON.stringify(n)} ${e}`); return `(.localVar ${JSON.stringify(n)})` }
  ctx.abiRequire = e => ctx.emit(`.require ${e} "ABI guard"`)
  return ctx
}
// Small independent executor for the actual emitted native CM subset, not a
// mock of the ABI functions. Arithmetic uses EVM word wrapping and byte memory.
function parse(text) {
  const ts = text.match(/"(?:[^"\\]|\\.)*"|[()\[\],]|[^\s()\[\],]+/g) ?? []; let i = 0
  function one() {
    const t = ts[i++]
    if (t === '(') { const x = []; while (ts[i] !== ')') x.push(one()); i++; return x }
    if (t === '[') { const x = []; while (ts[i] !== ']') { if (ts[i] === ',') i++; else x.push(stmt()) } i++; return x }
    if (t.startsWith('"')) return JSON.parse(t)
    return /^\d+$/.test(t) ? BigInt(t) : t
  }
  function stmt() { const x = [one()]; while (i < ts.length && ![',', ']'].includes(ts[i])) x.push(one()); return x }
  return one()
}
class VM {
  constructor(data = Buffer.alloc(0)) { this.data = data; this.mem = Buffer.alloc(1 << 20, 0xa5); this.vars = new Map(); this.store(64n, 4096n) }
  word(buf, p) { let n = 0n; for (let i = 0; i < 32; i++) n = (n << 8n) | BigInt(buf[Number(p) + i] ?? 0); return n }
  store(p, n) { assert(p >= 0 && p + 32n <= BigInt(this.mem.length), 'test memory capacity'); this.mem.set(Buffer.from((n & MASK).toString(16).padStart(64, '0'), 'hex'), Number(p)) }
  ev(x) {
    if (typeof x === 'bigint') return x
    if (Array.isArray(x) && x.length === 1 && Array.isArray(x[0])) return this.ev(x[0])
    const [op, ...args] = x
    if (op === '.literal') return args[0]
    if (op === '.localVar') { assert(this.vars.has(args[0]), `unbound ${args[0]}`); return this.vars.get(args[0]) }
    if (op === '.calldatasize') return BigInt(this.data.length)
    const [a, b] = args.map(y => this.ev(y))
    switch (op) {
      case '.mload': return this.word(this.mem, a)
      case '.calldataload': return this.word(this.data, a)
      case '.add': return (a + b) & MASK
      case '.sub': return (a - b) & MASK
      case '.mul': return (a * b) & MASK
      case '.div': return b ? a / b : 0n
      case '.mod': return b ? a % b : 0n
      case '.ceilDiv': return a === 0n ? 0n : (a - 1n) / b + 1n
      case '.min': return a < b ? a : b
      case '.bitAnd': return a & b
      case '.bitOr': return a | b
      case '.bitNot': return a ^ MASK
      case '.shl': return a >= 256n ? 0n : (b << a) & MASK
      case '.shr': return a >= 256n ? 0n : b >> a
      case '.byte': return a >= 32n ? 0n : (b >> ((31n - a) * 8n)) & 255n
      case '.signextend': { const bits = (a + 1n) * 8n; if (bits >= 256n) return b; const m = (1n << bits) - 1n; return b & (1n << (bits - 1n)) ? b | (MASK ^ m) : b & m }
      case '.logicalNot': return a ? 0n : 1n
      case '.eq': return BigInt(a === b)
      case '.lt': return BigInt(a < b)
      case '.le': return BigInt(a <= b)
      case '.ge': return BigInt(a >= b)
      default: throw Error(`unknown expression ${op}`)
    }
  }
  run(stmts) {
    for (const [op, ...args] of stmts) {
      if (op === '.letVar') this.vars.set(args[0], this.ev(args[1]))
      else if (op === '.require') assert(this.ev(args[0]) !== 0n, 'ABI guard')
      else if (op === '.requireError') {
        if (this.ev(args[0]) === 0n) throw Error(`${args[1]}(${args[2].map(a => this.ev(a)).join(',')})`)
      }
      else if (op === '.mstore') this.store(this.ev(args[0]), this.ev(args[1]))
      else if (op === '.calldatacopy') {
        const [d, s, n] = args.map(a => Number(this.ev(a)))
        for (let i = 0; i < n; i++) this.mem[d + i] = this.data[s + i] ?? 0
      } else if (op === '.forEach') {
        const n = this.ev(args[1]); assert(n < 10000n)
        for (let i = 0n; i < n; i++) { this.vars.set(args[0], i); this.run(args[2]) }
      } else throw Error(`unknown statement ${op}`)
    }
  }
  execute(c) { this.run(parse(`[${c.statements.join(',')}]`)) }
  result(v) { const p = this.ev(parse(v.ptr)), n = this.word(this.mem, p); return this.mem.subarray(Number(p + 32n), Number(p + 32n + n)) }
}
const word = n => Buffer.from((BigInt(n) & MASK).toString(16).padStart(64, '0'), 'hex')
const cat = xs => Buffer.concat(xs)
// Independent canonical reference encoder, including dynamic-element arrays.
function tuple(types, values) {
  const encoded = types.map((t, i) => ref(t, values[i]))
  let offset = encoded.reduce((n, b, i) => n + (abi.isDynamic(types[i]) ? 32 : b.length), 0)
  const heads = [], tails = []
  encoded.forEach((b, i) => { if (abi.isDynamic(types[i])) { heads.push(word(offset)); tails.push(b); offset += b.length } else heads.push(b) })
  return cat([...heads, ...tails])
}
function ref(t, v) {
  if (t.kind === 'scalar') return word(v)
  if (t.kind === 'bytes' || t.kind === 'string') { const b = Buffer.from(v, 'hex'); return cat([word(b.length), b, Buffer.alloc((32 - b.length % 32) % 32)]) }
  if (t.kind === 'struct') return tuple(t.members.map(m => m.type), v)
  if (t.kind === 'array') return cat([...(t.length === null ? [word(v.length)] : []), tuple(v.map(() => t.element), v)])
  throw Error(t.kind)
}
function view(t, place = 'calldata', ptr = 0) { return { type: t, place, ptr: L(ptr) } }
function checkEncoding(t, js, place = 'calldata') {
  const input = ref(t, js), c = context(), vm = new VM(input)
  if (place === 'memory') vm.mem.set(input, 128)
  const out = abi.encodeTuple([view(t, place, place === 'memory' ? 128 : 0)], c)
  vm.execute(c)
  assert.deepEqual(vm.result(out), tuple([t], [js]))
  const p = vm.ev(parse(out.ptr)), n = vm.word(vm.mem, p), free = vm.word(vm.mem, 64n)
  assert.equal(free % 32n, 0n)
  assert(vm.mem.subarray(Number(p + 32n + n), Number(free)).every(x => x === 0), 'zero ABI padding')
  return c
}
let tests = 0
function test(name, f) { f(); tests++; console.log(`ok ${name}`) }
const coll = { kind: 'struct', declaration: 99, members: [{ declaration: 1, name: 'same', type: A }, { declaration: 2, name: 'same', type: U }] }
const arr = { kind: 'array', length: null, element: coll }
const nested = { kind: 'struct', declaration: 100, members: [{ declaration: 3, type: arr }, { declaration: 4, type: bytes }, { declaration: 5, type: coll }] }
let sample

test('static layouts, member identity, type-width cleanup', () => {
  assert.equal(abi.headBytes(coll), 64); assert.equal(abi.headBytes(arr), 32)
  assert.equal(abi.headBytes({ kind: 'array', element: coll, length: '3' }), 192)
  const c = context(), v = abi.readMember(view(coll), 2, c), vm = new VM(ref(coll, [7, 9]))
  vm.execute(c); assert.equal(vm.ev(parse(v.expr)), 9n)
  assert.throws(() => abi.readMember(view(coll), 'same', context()), /declaration/)
  assert.equal(new VM().ev(parse(abi.cleanScalar(scalar(8), L(511), context()))), 255n)
  assert.equal(new VM().ev(parse(abi.cleanScalar(scalar(8, { signed: true, abi: 'int8' }), L(255), context()))), MASK)
})
test('dynamic arrays of static tuples, nested dynamic structs, dirty memory padding', () => {
  const value = [[[11, 12], [13, 14]], 'deadbeef', [15, 16]]
  sample = checkEncoding(nested, value)
  checkEncoding(nested, value, 'memory')
  checkEncoding(arr, []); checkEncoding({ kind: 'array', length: '2', element: coll }, [[1, 2], [3, 4]])
  for (const n of [0, 1, 31, 32, 33, 65]) { checkEncoding(bytes, 'ab'.repeat(n)); checkEncoding(bytes, 'ab'.repeat(n), 'memory') }
})
test('multicall bytes[] offsets relative to post-length head and bounds', () => {
  const t = { kind: 'array', element: bytes, length: null }, data = ref(t, ['aa', 'bbcc'])
  const c = context(), v = abi.readIndex(view(t), L(1), c), out = abi.encodePacked([v], c), vm = new VM(data)
  vm.execute(c); assert.equal(vm.result(out).toString('hex'), 'bbcc')
  const bad = context(); abi.readIndex(view(t), L(2), bad); assert.throws(() => new VM(data).execute(bad), /^Error: Panic\(50\)$/)
  assert(bad.statements.some(s => s.includes('.requireError') && s.includes('"Panic"')), 'source indexing must use Panic, not malformed-ABI revert')
  assert.throws(() => abi.encodeTuple([view(t)], context()), /dynamic elements/)
})
test('malformed calldata, narrow canonicality, wrapped offsets fail closed', () => {
  for (const data of [Buffer.alloc(31), cat([word(MASK), word(0)])]) {
    const c = context(); abi.byteLength(view(bytes), c); assert.throws(() => new VM(data).execute(c), /ABI guard/)
  }
  const c = context(); abi.readMember(view(coll), 1, c)
  assert.throws(() => new VM(cat([word(1n << 160n), word(0)])).execute(c), /ABI guard/)
  const t = { kind: 'struct', members: [{ declaration: 1, type: bytes }] }
  const d = context(); abi.byteLength(abi.readMember(view(t, 'calldata', 32), 1, d), d)
  assert.throws(() => new VM(cat([word(0), word(MASK)])).execute(d), /ABI guard/)
})
test('IdLib packed uint8/address/uint256/bytes concatenation and literal tails', () => {
  const c = context(), input = ref(bytes, 'cafe'), vm = new VM(input)
  const out = abi.encodePacked([{ type: scalar(8), place: 'scalar', expr: L(258) }, { type: A, place: 'scalar', expr: L(0x1234) }, { type: U, place: 'scalar', expr: L(9) }, view(bytes), { type: bytes, place: 'literal', hex: '0xabcdef' }], c)
  vm.execute(c)
  assert.equal(vm.result(out).toString('hex'), '02' + '1234'.padStart(40, '0') + '9'.padStart(64, '0') + 'cafeabcdef')
  const d = context(), literal = abi.encodeTuple([{ type: bytes, place: 'literal', hex: 'aa'.repeat(33) }], d), m = new VM(); m.execute(d)
  assert.deepEqual(m.result(literal), tuple([bytes], ['aa'.repeat(33)]))
})
test('fixed arrays of dynamic bytes index correctly; bytes1 alignment and memory packing', () => {
  const t = { kind: 'array', length: '2', element: bytes }, input = ref(t, ['aa', 'bbcc'])
  const c = context(), item = abi.readIndex(view(t), L(1), c), b = abi.readIndex(item, L(1), c)
  const out = abi.encodePacked([b], c), vm = new VM(input)
  vm.execute(c); assert.equal(vm.result(out).toString('hex'), 'cc')
  const d = context(), m = new VM()
  m.mem.set(ref(bytes, 'ab'.repeat(33)), 128)
  const result = abi.encodePacked([{ type: scalar(8), place: 'scalar', expr: L(1) }, view(bytes, 'memory', 128), { type: scalar(16, { signed: true, abi: 'int16' }), place: 'scalar', expr: L(MASK) }], d)
  m.execute(d); assert.equal(m.result(result).toString('hex'), '01' + 'ab'.repeat(33) + 'ffff')
  // Include packing and memory-copy constructors in the Lean sample too.
  sample.statements.push(...d.statements)
})
if (!process.argv.includes('--no-source')) {
  test('pinned source-derived CollateralParams/Market/Offer layouts and encodings', () => {
    const config = JSON.parse(readFileSync(resolve(root, 'config/midnight-admin-import.json'), 'utf8'))
    const source = new MidnightSource(compilePinnedSource(root, config), config)
    const structs = [...source.nodes.values()].filter(n => n.nodeType === 'StructDefinition' && ['CollateralParams', 'Market', 'Offer'].includes(n.name))
    assert(structs.some(n => n.name === 'CollateralParams'))
    const value = t => t.kind === 'scalar' ? (t.abi === 'bool' ? 1 : 7) : ['bytes', 'string'].includes(t.kind) ? 'cafe' : t.kind === 'array' ? Array.from({ length: t.length === null ? 2 : Number(t.length) }, () => value(t.element)) : t.members.map(m => value(m.type))
    for (const n of structs) {
      const t = { kind: 'struct', declaration: n.id, members: n.members.map(m => ({ declaration: m.id, type: source.type(m) })) }
      checkEncoding(t, value(t)); console.log(`  source ${n.name}: ${source.abiType(t)}, head=${abi.headBytes(t)}`)
    }
    assert(structs.length >= 3, 'required source structs found')
  })
}
if (process.argv.includes('--lean')) {
  test('generated native CM sample Lean elaboration', () => {
    const dir = mkdtempSync(resolve(tmpdir(), 'midnight-abi-'))
    try {
      const path = resolve(dir, 'AbiSample.lean')
      writeFileSync(path, `import Verity.Core.Model.Types\nopen Compiler.CompilationModel\nset_option maxRecDepth 10000\nset_option maxHeartbeats 2000000\ndef abiSample : List Stmt := [${sample.statements.join(',\n')}]\n#check abiSample\n`)
      const run = spawnSync('lake', ['env', 'lean', path], { cwd: root, encoding: 'utf8', timeout: 120000, env: { ...process.env, HOME: '/home/claudine', ELAN_HOME: '/home/claudine/.elan' } })
      assert.equal(run.status, 0, `${run.error ?? ''}\n${run.stdout}\n${run.stderr}`)
      console.log(run.stdout.trim())
    } finally { rmSync(dir, { recursive: true, force: true }) }
  })
}
console.log(`${tests} ABI tests passed`)
