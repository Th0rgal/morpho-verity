/** Generic source-type ABI views and native CompilationModel emission.
 * TRUST BOUNDARY: composite memory values are ABI images, NOT Solidity heap
 * objects. Caller owns valid, non-overlapping memory views and initializes 0x40
 * to an aligned free address >= 0x80. Calldata reads are range/canonical checked;
 * this is a lazy decoder, not a claim of whole-input solc decoder equivalence.
 * ctx.abiRequire must terminate/revert on false (panic/error policy is caller's).
 * Expressions are Lean Expr text, statements Lean Stmt text (no ECM/templates).
 */
const E = (op, ...args) => `(.${op}${args.length ? ' ' + args.join(' ') : ''})`
const L = n => E('literal', String(n))
const add = (a, b) => E('add', a, b)
const mul = (a, b) => E('mul', a, b)
const sub = (a, b) => E('sub', a, b)
const W = L(32), ZERO = L(0)
const BYTES = Object.freeze({ kind: 'bytes', abi: 'bytes' })
const MAX = (1n << 256n) - 1n
function fail(ctx, reason, node) { ctx?.fail?.(reason, node); throw new Error(reason) }
function sizeNumber(n) {
  if (n > BigInt(Number.MAX_SAFE_INTEGER)) throw new Error('ABI static size exceeds safe generator range')
  return Number(n)
}
export function isDynamic(t) {
  switch (t.kind) {
    case 'scalar': return false
    case 'bytes': case 'string': return true
    case 'array': return t.length === null || isDynamic(t.element)
    case 'struct': return t.members.some(m => isDynamic(m.type))
    default: throw new Error(`non-ABI type ${t.kind}`)
  }
}
/** Number of bytes occupied in the parent's head. */
export function headBytes(t) {
  if (isDynamic(t)) return 32
  if (t.kind === 'scalar') return 32
  if (t.kind === 'array') return sizeNumber(BigInt(t.length) * BigInt(headBytes(t.element)))
  return sizeNumber(t.members.reduce((n, m) => n + BigInt(headBytes(m.type)), 0n))
}
function checkedAdd(a, b, ctx) {
  const x = ctx.bind(add(a, b), 'abi_add')
  ctx.abiRequire(E('ge', x, a)); return x
}
function checkedMul(a, n, ctx) {
  ctx.abiRequire(E('le', a, L(MAX / BigInt(n))))
  return ctx.bind(mul(a, L(n)), 'abi_size')
}
function rounded(n, ctx) {
  return E('bitAnd', checkedAdd(n, L(31), ctx), L(MAX - 31n))
}
function range(value, ptr, n, ctx) {
  const end = checkedAdd(ptr, n, ctx)
  if (value.place === 'calldata') ctx.abiRequire(E('le', end, E('calldatasize')))
  else if (value.place !== 'memory') fail(ctx, `ABI pointer needs memory/calldata, got ${value.place}`)
}
function load(v, ptr, ctx) {
  range(v, ptr, W, ctx)
  return ctx.bind(E(v.place === 'calldata' ? 'calldataload' : 'mload', ptr), 'abi_word')
}
export function cleanScalar(type, expr, ctx) {
  if (type.kind !== 'scalar' || !Number.isInteger(type.bits) || type.bits < 8 || type.bits > 256 || type.bits % 8) fail(ctx, 'invalid ABI scalar type')
  if (type.abi === 'bool') return E('logicalNot', E('logicalNot', expr))
  if (type.signed) return E('signextend', L(type.bits / 8 - 1), expr)
  if (type.bits === 256) return expr
  const mask = type.alignment === 'left' ? ((1n << BigInt(type.bits)) - 1n) << BigInt(256 - type.bits) : (1n << BigInt(type.bits)) - 1n
  return E('bitAnd', expr, L(mask))
}
function at(type, place, ptr, ctx) {
  const v = { type, place, ptr }
  if (type.kind !== 'scalar') return v
  const raw = load(v, ptr, ctx), expr = cleanScalar(type, raw, ctx)
  if (place === 'calldata') ctx.abiRequire(E('eq', raw, expr))
  return { type, place: 'scalar', expr }
}
function project(v, type, base, slot, ctx) {
  const ptr = isDynamic(type) ? checkedAdd(base, load(v, slot, ctx), ctx) : slot
  return at(type, v.place, ptr, ctx)
}
export function readMember(value, memberDeclarationID, ctx) {
  if (value.type.kind !== 'struct') fail(ctx, 'ABI member on non-struct')
  let offset = 0
  for (const m of value.type.members) {
    if (m.declaration === memberDeclarationID) return project(value, m.type, value.ptr, checkedAdd(value.ptr, L(offset), ctx), ctx)
    offset += headBytes(m.type)
  }
  fail(ctx, `ABI member declaration ${memberDeclarationID} not in struct ${value.type.declaration}`)
}
export function byteLength(value, ctx) {
  if (!['bytes', 'string'].includes(value.type.kind)) fail(ctx, 'byteLength needs bytes/string')
  if (value.place === 'literal') return L(literalHex(value, ctx).length / 2)
  const n = load(value, value.ptr, ctx)
  range(value, checkedAdd(value.ptr, W, ctx), n, ctx)
  return n
}
export function byteData(value, ctx) {
  if (!['bytes', 'string'].includes(value.type.kind) || value.place === 'literal') fail(ctx, 'byteData needs ABI memory/calldata bytes')
  return checkedAdd(value.ptr, W, ctx)
}
export function arrayLength(value, ctx) {
  if (value.type.kind !== 'array') fail(ctx, 'arrayLength needs array')
  return value.type.length === null ? load(value, value.ptr, ctx) : L(value.type.length)
}
// Source-level array/bytes indexing is Panic(0x32), distinct from a malformed
// ABI decode's empty revert. Do not route runtime index bounds through abiRequire.
function indexRequire(condition, ctx) {
  if (ctx.indexRequire) ctx.indexRequire(condition)
  else ctx.emit(`.requireError ${condition} "Panic" [${L(0x32)}]`)
}
export function readIndex(value, indexExpr, ctx) {
  const i = ctx.bind(indexExpr, 'abi_index'), t = value.type
  if (['bytes', 'string'].includes(t.kind)) {
    if (t.kind === 'string') fail(ctx, 'Solidity string indexing unsupported')
    if (value.place === 'literal') fail(ctx, 'literal bytes indexing requires materialization')
    indexRequire(E('lt', i, byteLength(value, ctx)), ctx)
    const ptr = checkedAdd(byteData(value, ctx), i, ctx)
    // Only one byte is semantically read; calldataload zero-padding is valid.
    range(value, ptr, L(1), ctx)
    return { type: { kind: 'scalar', abi: 'bytes1', bits: 8, signed: false, alignment: 'left' }, place: 'scalar', expr: E('bitAnd', E(value.place === 'calldata' ? 'calldataload' : 'mload', ptr), L(255n << 248n)) }
  }
  if (t.kind !== 'array') fail(ctx, 'ABI indexing needs array/bytes')
  const n = arrayLength(value, ctx)
  indexRequire(E('lt', i, n), ctx)
  const base = t.length === null ? checkedAdd(value.ptr, W, ctx) : value.ptr
  const slot = checkedAdd(base, checkedMul(i, headBytes(t.element), ctx), ctx)
  return project(value, t.element, base, slot, ctx)
}
function literalHex(v, ctx) {
  const h = v.hex?.replace(/^0x/, '')
  if (typeof h !== 'string' || !/^(?:[0-9a-fA-F]{2})*$/.test(h)) fail(ctx, 'invalid literal bytes hex')
  return h
}
function scoped(ctx, fn) {
  // Preserve the core emitter's statement-local unsafe annotations and scope
  // handling. Replacing ctx.emit bypasses those hooks inside generated loops.
  if (typeof ctx.capture === 'function') {
    const captured = ctx.capture(() => fn(ctx))
    return `[${captured.body.join(',\n')}]`
  }
  const stmts = []
  // Capture through the original context: bind/abiRequire may close over ctx.
  // They must route statements through ctx.emit, rather than a private sink.
  const old = ctx.emit
  ctx.emit = s => stmts.push(s)
  try { fn(ctx) } finally { ctx.emit = old }
  return `[${stmts.join(',\n')}]`
}
function loop(count, ctx, fn) {
  const name = ctx.fresh('abi_i')
  const body = scoped(ctx, child => fn(E('localVar', JSON.stringify(name)), child))
  ctx.emit(`.forEach ${JSON.stringify(name)} ${count} ${body}`)
}
/** Exact byte copy into disjoint destination; preserves bytes after its end.
 * Memory views must be disjoint from destination (not memmove).
 */
export function copyBytes(value, dest, ctx) {
  const n = byteLength(value, ctx)
  if (value.place === 'literal') {
    const h = literalHex(value, ctx)
    for (let i = 0; i < h.length; i += 64) {
      const chunk = h.slice(i, i + 64), mask = L(MAX ^ ((1n << BigInt(256 - chunk.length * 4)) - 1n))
      const p = add(dest, L(i / 2))
      ctx.emit(`.mstore ${p} ${E('bitOr', L(BigInt('0x' + chunk.padEnd(64, '0'))), E('bitAnd', E('mload', p), E('bitNot', mask)))}`)
    }
  } else if (value.place === 'calldata') ctx.emit(`.calldatacopy ${dest} ${byteData(value, ctx)} ${n}`)
  else {
    const src = byteData(value, ctx)
    loop(E('ceilDiv', n, W), ctx, (i, c) => {
      const off = mul(i, W), p = add(dest, off)
      const bits = mul(sub(W, E('min', W, sub(n, off))), L(8))
      const mask = E('shl', bits, L(MAX))
      c.emit(`.mstore ${p} ${E('bitOr', E('bitAnd', E('mload', add(src, off)), mask), E('bitAnd', E('mload', p), E('bitNot', mask)))}`)
    })
  }
  return n
}
function supported(t, ctx) {
  isDynamic(t)
  if (t.kind === 'array') {
    if (isDynamic(t.element)) fail(ctx, 'ABI encoding arrays with dynamic elements is not supported')
    supported(t.element, ctx)
  } else if (t.kind === 'struct') t.members.forEach(m => supported(m.type, ctx))
}
function encodedSize(v, ctx) {
  const t = v.type
  if (!isDynamic(t)) return L(headBytes(t))
  if (['bytes', 'string'].includes(t.kind)) return checkedAdd(W, rounded(byteLength(v, ctx), ctx), ctx)
  if (t.kind === 'array') return checkedAdd(W, checkedMul(arrayLength(v, ctx), headBytes(t.element), ctx), ctx)
  let n = L(t.members.reduce((s, m) => s + headBytes(m.type), 0))
  for (const m of t.members) if (isDynamic(m.type)) n = checkedAdd(n, encodedSize(readMember(v, m.declaration, ctx), ctx), ctx)
  return n
}
function writeTuple(values, dest, ctx, load = value => value) {
  let head = 0, tail = L(values.reduce((s, v) => s + headBytes(v.type), 0))
  for (const entry of values) {
    const v = load(entry)
    const target = add(dest, L(head))
    if (isDynamic(v.type)) {
      ctx.emit(`.mstore ${target} ${tail}`)
      writeValue(v, add(dest, tail), ctx)
      tail = checkedAdd(tail, encodedSize(v, ctx), ctx)
    } else writeValue(v, target, ctx)
    head += headBytes(v.type)
  }
}
function writeValue(v, dest, ctx) {
  const t = v.type
  if (t.kind === 'scalar') {
    const x = v.expr ?? at(t, v.place, v.ptr, ctx).expr
    ctx.emit(`.mstore ${dest} ${cleanScalar(t, x, ctx)}`)
  } else if (['bytes', 'string'].includes(t.kind)) {
    const n = byteLength(v, ctx), data = add(dest, W)
    ctx.emit(`.mstore ${dest} ${n}`)
    copyBytes(v, data, ctx)
  } else if (t.kind === 'struct') {
    // Destination is a fresh ABI allocation. Stream fields in source order
    // rather than keeping every decoded scalar live until encoding begins.
    writeTuple(t.members, dest, ctx, member => readMember(v, member.declaration, ctx))
  }
  else if (t.kind === 'array') {
    const n = arrayLength(v, ctx), data = t.length === null ? add(dest, W) : dest
    if (t.length === null) ctx.emit(`.mstore ${dest} ${n}`)
    loop(n, ctx, (i, c) => writeValue(readIndex(v, i, c), add(data, mul(i, L(headBytes(t.element)))), c))
  } else fail(ctx, `cannot ABI encode ${t.kind}`)
}
function allocateBytes(n, ctx) {
  const ptr = ctx.bind(E('mload', L(64)), 'abi_alloc')
  ctx.abiRequire(E('ge', ptr, L(128)))
  ctx.abiRequire(E('eq', E('mod', ptr, W), ZERO))
  const payload = rounded(n, ctx), total = checkedAdd(W, payload, ctx)
  const end = checkedAdd(ptr, total, ctx)
  ctx.emit(`.mstore ${L(64)} ${end}`)
  ctx.emit(`.mstore ${ptr} ${n}`)
  // Clear payload for deterministic padding even when free memory was dirty.
  loop(E('div', payload, W), ctx, (i, c) => c.emit(`.mstore ${add(add(ptr, W), mul(i, W))} ${ZERO}`))
  return { type: BYTES, place: 'memory', ptr }
}
export function encodeTuple(values, ctx) {
  values.forEach(v => supported(v.type, ctx))
  let n = L(values.reduce((s, v) => s + headBytes(v.type), 0))
  for (const v of values) if (isDynamic(v.type)) n = checkedAdd(n, encodedSize(v, ctx), ctx)
  const out = allocateBytes(n, ctx)
  writeTuple(values, byteData(out, ctx), ctx)
  return out
}
export function encodePacked(values, ctx) {
  for (const v of values) if (!['scalar', 'bytes', 'string'].includes(v.type.kind)) fail(ctx, 'packed encoding supports only scalars/bytes/string (no array/struct)')
  const widths = values.map(v => v.type.kind === 'scalar' ? L(v.type.bits / 8) : byteLength(v, ctx))
  let n = ZERO
  for (const w of widths) n = checkedAdd(n, w, ctx)
  const out = allocateBytes(n, ctx)
  let dest = byteData(out, ctx)
  values.forEach((v, i) => {
    if (v.type.kind === 'scalar') {
      const width = v.type.bits / 8
      const x = ctx.bind(cleanScalar(v.type, v.expr ?? at(v.type, v.place, v.ptr, ctx).expr, ctx), 'abi_packed_scalar')
      // Byte-at-a-time masked word stores avoid writing past allocation end.
      for (let j = 0; j < width; j++) {
        const pos = add(dest, L(j)), ix = v.type.alignment === 'left' ? j : 32 - width + j
        const p = E('bitAnd', pos, L(MAX - 31n))
        const shift = sub(L(248), mul(E('mod', pos, W), L(8)))
        const b = E('shl', shift, E('byte', L(ix), x))
        const mask = E('bitNot', E('shl', shift, L(255)))
        ctx.emit(`.mstore ${p} ${E('bitOr', b, E('bitAnd', E('mload', p), mask))}`)
      }
    } else copyBytes(v, dest, ctx)
    dest = checkedAdd(dest, widths[i], ctx)
  })
  return out
}
