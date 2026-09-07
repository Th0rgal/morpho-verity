import { SourceError, walk } from './midnight-source.mjs'
import { sourceCEIPolicies } from './midnight-cei.mjs'
import { keccak256 } from './keccak256.mjs'

const q = JSON.stringify
// Keep generated list notation shallow. Very large literal lists overflow the
// pinned Lean elaborator's default thread stack even when recursion limits rise.
// Chunking changes only construction of the same ordered list, not CM semantics.
const list = xs => {
  if (xs.length <= 24) return `[${xs.join(',\n')}]`
  const chunks = []
  for (let i = 0; i < xs.length; i += 24) chunks.push(`[${xs.slice(i, i + 24).join(',\n')}]`)
  return `(List.flatten ${list(chunks)})`
}
const e = (op, ...args) => `(.${op}${args.length ? ' ' + args.join(' ') : ''})`
const lit = n => e('literal', BigInt.asUintN(256, BigInt(n)).toString())
const uint = bits => ({ kind: 'scalar', abi: `uint${bits}`, bits, signed: false })
const bool = { kind: 'scalar', abi: 'bool', bits: 8, signed: false }
const scalar = (type, expr) => ({ type, place: 'scalar', expr })
const packed = (offset, width) => `{ offset := ${offset}, width := ${width} }`
const pathKey = steps => steps.map(s => s.kind === 'member' ? `m${s.declaration}` : `i${s.index}`).join('_') || 'value'

/** AST-only core backend. No protocol-body templates and no catch-and-skip generation.
 *
 * generate() returns {lean, fields, errors, events, constructor, functions, origins,
 * policies}. It is all-or-error. compileFunction(decl) returns a FunctionSpec text
 * (ConstructorSpec for constructor), useful for explicitly labelled partial probes.
 *
 * Extension hooks: options.abi implements the agreed midnight-abi Value API;
 * options.call(node, ctx) / options.statement(node, ctx) may lower explicit boundary
 * nodes, but must throw on unsupported nodes. ctx is a FunctionContext: emit/fresh/
 * bind/fail/abiRequire, expression, value, statements and owner/source are available.
 * Hooks must return a Value (call) or true (statement). No fallback values exist.
 * Composite helpers use scalar uint256 ABI-memory-view pointers; public wrappers
 * retain source ABI parameter types and calldata views. Storage-reference helper
 * parameters fail closed. ABI memory views are readonly except explicit inline Yul.
 * Narrow ParamType output words retain frontend types in Values and source ABI must
 * remain the ABI metadata authority (the pin lacks uint32/uint128 ParamType).
 * options.factorStatements opts into transparent native Stmt definitions to bound
 * Lean elaborator term size. statementOrigins records each definition's source
 * declaration/policy; packagers must explicitly inventory them before opting in.
 */
export class MidnightLowering {
  constructor(source, options = {}) {
    this.source = source
    this.options = options
    this.ceiPolicies = sourceCEIPolicies(source)
    this.fields = []
    this.storage = new Map()
    this.origins = []
    this.functionMutability = new Map()
    this.policies = ['immutable-to-reserved-storage-1024-v1', 'source-ABI-authoritative-narrow-word-v1', 'internal-composite-ABI-memory-pointer-v1', 'structurally-validated-bitmap-width-loop-v1', 'source-opcode-return-assumed-v1', 'source-low-level-boundary-v1', 'source-order-CEI-compilation-exception-v1', 'source-pure-backend-conservative-view-v1']
    this.prepareStorage()
  }
  fail(reason, node) { throw new SourceError(reason, node) }
  paramType(type) {
    if (type.kind === 'scalar') {
      if (['bool', 'address', 'bytes32', 'uint8', 'uint16'].includes(type.abi)) return `.${type.abi}`
      if (/^u?int/.test(type.abi)) return type.signed ? '.int256' : '.uint256'
      this.fail(`ParamType cannot represent ${type.abi}`)
    }
    if (['bytes', 'string'].includes(type.kind)) return `.${type.kind}`
    if (type.kind === 'struct') return `(.tuple ${list(type.members.map(m => this.paramType(m.type)))})`
    if (type.kind === 'array') return type.length === null ? `(.array ${this.paramType(type.element)})` : `(.fixedArray ${this.paramType(type.element)} ${type.length})`
    this.fail(`non-ABI parameter ${type.kind}`)
  }
  functionName(decl, internal = false) { return !internal && ['public', 'external'].includes(decl.visibility) ? decl.name : `fn_${decl.id}` }
  prepareStorage() {
    const source = this.source
    const keyType = t => {
      if (!['address', 'bytes32'].includes(t.abi) && !/^uint/.test(t.abi)) this.fail(`unsupported mapping key ${t.abi}`)
      return `.${['address', 'bytes32'].includes(t.abi) ? t.abi : 'uint256'}`
    }
    for (const [id, root] of source.state) {
      const decl = source.declaration(id), name = `s_${id}`
      let type = source.type(decl), keys = []
      while (type.kind === 'mapping') { keys.push(type.key); type = type.value }
      if (keys.length > 2) this.fail('more than two storage mapping keys', decl)
      const leaves = []
      const descend = (t, steps) => {
        if (t.kind === 'struct') for (const m of t.members) descend(m.type, [...steps, { kind: 'member', declaration: m.declaration }])
        else if (t.kind === 'array' && t.length !== null) {
          if (BigInt(t.length) > 4096n) this.fail('fixed array exceeds lowering resource bound', decl)
          for (let i = 0; i < Number(t.length); i++) descend(t.element, [...steps, { kind: 'index', index: String(i), expression: { op: 'literal', value: String(i) } }])
        } else if (t.kind === 'scalar') {
          const all = [...keys.map((_, i) => ({ kind: 'index', expression: { op: 'key', index: i } })), ...steps]
          const plan = source.storagePath(id, all)
          // Strip mapping hashes, leaving the literal word offset relative to anchor.
          const evaluate = x => {
            if (x.op === 'mappingHash') return 0n
            if (x.op === 'literal') return BigInt(x.value)
            const a = evaluate(x.left), b = evaluate(x.right)
            if (x.op === 'add') return a + b
            if (x.op === 'mul') return a * b
            if (x.op === 'div') return a / b
            if (x.op === 'mod') return a % b
            this.fail(`nonstatic storage leaf ${x.op}`, decl)
          }
          leaves.push({ steps, type: t, name: pathKey(steps), word: evaluate(plan.address) - (keys.length ? 0n : BigInt(root.slot)), offset: evaluate(plan.offset) * 8n, width: t.bits })
        } else this.fail(`unsupported storage leaf ${t.kind}`, decl)
      }
      descend(type, [])
      let ty, bits = ''
      if (!keys.length) {
        if (leaves.length !== 1 || leaves[0].steps.length) this.fail('nonmapping aggregate storage not yet lowered', decl)
        ty = type.abi === 'address' ? '.address' : '.uint256'
        bits = `, packedBits := some ${packed(leaves[0].offset, leaves[0].width)}`
      } else {
        const members = list(leaves.map(l => `{ name := ${q(l.name)}, wordOffset := ${l.word}, packed := some ${packed(l.offset, l.width)} }`))
        ty = `(.mappingStruct${keys.length === 2 ? '2' : ''} ${keys.map(keyType).join(' ')} ${members})`
      }
      this.fields.push(`{ name := ${q(name)}, ty := ${ty}, slot := some ${root.slot}${bits} }`)
      this.storage.set(id, { name, keys, leaves, type: source.type(decl) })
    }
    const immutables = source.declarations().filter(d => d.nodeType === 'VariableDeclaration' && d.mutability === 'immutable')
    for (let i = 0; i < immutables.length; i++) {
      const decl = immutables[i], slot = 1024n + BigInt(i), type = source.type(decl)
      if (type.kind !== 'scalar') this.fail('composite immutable', decl)
      for (const root of source.state.values()) if (slot >= BigInt(root.slot) && slot < BigInt(root.slot) + (BigInt(root.physical.bytes) + 31n) / 32n) this.fail('immutable reserved slot collision', decl)
      const name = `s_${decl.id}`
      this.fields.push(`{ name := ${q(name)}, ty := .uint256, slot := some ${slot}, packedBits := some ${packed(0, type.bits)} }`)
      this.storage.set(decl.id, { name, keys: [], leaves: [{ steps: [], type, name: 'value', word: 0n, offset: 0n, width: type.bits }], type, immutable: true })
    }
  }
  errorDef(decl) { return `{ name := ${q(decl.name)}, params := ${list(decl.parameters.parameters.map(p => this.paramType(this.source.type(p))))} }` }
  eventDef(decl) {
    if (decl.anonymous) this.fail('anonymous event', decl)
    return `{ name := ${q(decl.name)}, params := ${list(decl.parameters.parameters.map(p => `{ name := ${q(p.name)}, ty := ${this.paramType(this.source.type(p))}, kind := .${p.indexed ? 'indexed' : 'unindexed'} }`))} }`
  }
  compileFunction(decl, { internal = false } = {}) {
    const ctx = new FunctionContext(this, decl)
    ctx.internal = internal
    const result = ctx.compile()
    this.origins.push({ ...this.source.origin(decl), generatedName: this.functionName(decl, internal), callingConvention: internal || !['public', 'external'].includes(decl.visibility) ? 'scalar-or-ABI-memory-pointer-v1' : 'source-ABI-calldata-v1' })
    return result
  }
  generate() {
    if (this.options.factorStatements) { this.statementDefinitions = []; this.statementOrigins = [] }
    try { return this.generateModel() } finally { delete this.statementDefinitions; delete this.statementOrigins }
  }
  generateModel() {
    const surface = this.source.surface(), functions = [], errors = [], events = []
    let constructor = null
    for (const row of surface) {
      const decl = this.source.declaration(row.declaration)
      if (row.abi.type === 'error') errors.push(this.errorDef(decl))
      if (row.abi.type === 'event') events.push(this.eventDef(decl))
      if (row.abi.type === 'function' && decl.nodeType === 'VariableDeclaration') functions.push(this.compileGetter(decl))
    }
    errors.push('{ name := "Panic", params := [.uint256] }')
    for (const row of this.source.callGraph()) {
      const decl = this.source.declaration(row.declaration), code = this.compileFunction(decl)
      if (decl.kind === 'constructor') constructor = code
      else functions.push(code)
      // Verity internalCall requires an isInternal target, even for Solidity public helpers.
      if (decl.visibility === 'public' && this.source.callGraph().some(r => r.calls.some(c => c.target === decl.id))) functions.push(this.compileFunction(decl, { internal: true }))
    }
    // Separate definitions avoid elaborating the entire contract as one giant term.
    const definitions = functions.map((f, i) => `def sourceFunction_${i} : FunctionSpec := ${f.replaceAll('\n', ' ')}`).join('\n')
    const model = `{ name := ${q(this.source.contract.name)}, fields := ${list(this.fields)}, constructor := ${constructor ? `some (${constructor})` : 'none'}, functions := ${list(functions.map((_, i) => `sourceFunction_${i}`))}, events := ${list(events)}, errors := ${list(errors)} }`
    const lean = `-- Generated from typed Solidity AST; no handwritten model dependency.\nimport Compiler.CompilationModel\nnamespace Midnight.Generated.Full\nopen Compiler.CompilationModel\n${this.options.supportDefinitions?.() ?? ''}\ndef midnightOpcode_return : Compiler.ECM.ExternalCallModule where\n  name := \"source.opcode.return\"\n  numArgs := 2\n  resultVars := []\n  writesState := false\n  readsState := false\n  proofStatus := .assumed\n  axioms := [\"evm_return_exact_memory_bytes\"]\n  compile := fun _ args => if args.length == 2 then pure [.expr (.call \"return\" args)] else throw \"return arity\"\nset_option maxRecDepth 100000\nset_option maxHeartbeats 0\n${this.statementDefinitions?.join('\n') ?? ''}\n${definitions}\ndef spec : CompilationModel := ${model.replaceAll('\n', ' ')}\nend Midnight.Generated.Full\n`
    return { lean, fields: this.fields, errors, events, constructor, functions, origins: this.origins, statementOrigins: this.statementOrigins ?? [], policies: this.policies }
  }
  compileGetter(decl) {
    const ctx = new FunctionContext(this, decl), params = [], steps = []
    let type = this.source.type(decl), i = 0
    while (['mapping', 'array'].includes(type.kind)) {
      const key = type.kind === 'mapping' ? type.key : uint(256), name = `arg_${i++}`
      params.push(`{ name := ${q(name)}, ty := ${this.paramType(key)} }`)
      const expr = e('param', q(name))
      if (type.kind === 'array') ctx.panicUnless(e('lt', expr, lit(type.length)), 0x32)
      steps.push({ kind: 'index', expr })
      type = type.kind === 'mapping' ? type.value : type.element
    }
    const root = { place: 'storage', declaration: decl.id, steps, type }
    const values = type.kind === 'struct' ? type.members.filter(m => !['mapping', 'array'].includes(m.type.kind)).map(m => ctx.value({ ...root, type: m.type, steps: [...steps, { kind: 'member', declaration: m.declaration }] })) : [ctx.value(root)]
    // Evaluate storage expressions before backend return packing begins:
    // mappingSlot uses scratch words 0/32, also used by returnValues output.
    const frozen = values.map(v => ctx.bind(v.expr, 'getter_return'))
    ctx.emit(`.returnValues ${list(frozen)}`)
    return `{ name := ${q(decl.name)}, params := ${list(params)}, returnType := none, returns := ${list(values.map(v => this.paramType(v.type)))}, isView := true, body := ${list(ctx.body)} }`
  }
}

export class FunctionContext {
  constructor(owner, decl) {
    this.owner = owner; this.source = owner.source; this.decl = decl
    this.body = []; this.env = new Map(); this.serial = 0; this.unchecked = false
    this.backendPurityReasons = new Set()
  }
  fail(reason, node = this.decl) { this.owner.fail(reason, node) }
  emit(stmt) {
    // Inspect before factoring hides nested statements. Strings are not CM nodes.
    // This is a declared representation boundary, NOT a source-purity proof.
    if (this.decl.stateMutability === 'pure') {
      const syntax = stmt.replace(/"(?:[^"\\]|\\.)*"/g, '""')
      const forbidden = /\.(?:storage(?:Addr|ArrayLength|ArrayElement)?|mapping\w*|structMember2?|adtTag|adtField|caller|contractAddress|txOrigin|chainid|msgValue|selfBalance|blockTimestamp|blockNumber|blobbasefee|calldatasize|tload|extcodesize|call|staticcall|delegatecall|returndataSize|returndataOptionalBoolAt|returndataCopy|revertReturndata|returnStorageWords|returnCodeData|externalCallBind|tryExternalCallBind|ecm|externalCall)\b/
      if (forbidden.test(syntax)) this.fail('unacknowledged state/environment read in source-pure lowering')
      const unsafe = [...syntax.matchAll(/\.unsafeYul\b/g)].length
      const rawReverts = [...syntax.matchAll(/UnsafeYulFragment\.rawRevert\b/g)].length
      if (unsafe !== rawReverts) this.fail('unacknowledged unsafe fragment in source-pure lowering')
      if (rawReverts) this.backendPurityReasons.add('raw-revert-mechanics: exact empty/payload revert is conservatively an environment read; mechanics and assumed obligations retained')
      if (/\.(?:calldataload|calldatacopy)\b/.test(syntax)) this.backendPurityReasons.add('ABI-calldata-scaffolding: source argument decoding/copy is conservatively an environment read')
      for (const call of stmt.matchAll(/\.internalCall(?:Assign\s+\[[^\]]*\])?\s+("(?:[^"\\]|\\.)*")/g)) {
        const name = JSON.parse(call[1])
        const id = /^fn_(\d+)$/.exec(name)?.[1]
        const target = id ? this.source.declaration(Number(id)) : null
        if (!target || target.nodeType !== 'FunctionDefinition' || target.stateMutability !== 'pure') this.fail(`unacknowledged non-pure internal target ${name}`)
        this.backendPurityReasons.add(`source-pure-internal-helper: ${name}; conservative local classification and inferred callee scaffolding effects; not a callee-purity theorem`)
      }
    }
    // Only annotate individual low-level statements, never a source business body.
    // Children of control statements have already passed through emit. Hoist a
    // low-level condition/count into its own annotated binding, preserving order.
    const mechanics = /\.(?:call|staticcall|delegatecall|returndataSize|returndataCopy|revertReturndata|returndataOptionalBoolAt|mload|mstore|calldataload|calldatacopy|codecopy|extcodesize|extcodecopy|create2|tload|tstore)\b/
    if (/^\.(?:ite|forEach)\b/.test(stmt)) {
      let depth = 0, quoted = false, escaped = false, start = -1, end = -1
      for (let i = 0; i < stmt.length; i++) {
        const c = stmt[i]
        if (quoted) { if (escaped) escaped = false; else if (c === '\\') escaped = true; else if (c === '"') quoted = false; continue }
        if (c === '"') { quoted = true; continue }
        if (c === '(') { if (start < 0) start = i; depth++ }
        if (c === ')' && --depth === 0) { end = i + 1; break }
      }
      if (start < 0 || end < 0) this.fail('control expression rendering boundary missing')
      const expr = stmt.slice(start, end)
      if (mechanics.test(expr)) stmt = stmt.slice(0, start) + this.bind(expr, 'boundary_condition') + stmt.slice(end)
      this.pushStatement(stmt); return
    }
    if (mechanics.test(stmt) && !stmt.startsWith('.unsafeBlock ')) {
      // unsafeBlock is a lexical scope in the pinned backend. Keep the binding
      // outside it and only assign inside, so subsequent statements can use it.
      const binding = /^\.letVar ("(?:[^"\\\\]|\\\\.)*") (.*)$/s.exec(stmt)
      if (binding) {
        this.pushStatement(`.letVar ${binding[1]} ${lit(0)}`)
        stmt = `.assignVar ${binding[1]} ${binding[2]}`
      }
      const reason = `source-low-level-boundary-v1: declaration ${this.decl.id} (${this.decl.src}); ABI memory/calldata or source assembly/external opcode; assumed memory validity and EVM opcode refinement, not business-logic correctness`
      stmt = `.unsafeBlock ${q(reason)} [${stmt}]`
    }
    this.pushStatement(stmt)
  }
  pushStatement(stmt) {
    // Definitions are transparent native CM values, not opaque ECM bodies.
    // Factoring bounds elaborator work per term while preserving list order.
    if (this.owner.statementDefinitions) {
      const name = `sourceStatement_${this.owner.statementDefinitions.length}`
      this.owner.statementDefinitions.push(`def ${name} : Stmt := ${stmt.replaceAll('\n', ' ')}`)
      this.owner.statementOrigins.push({ ...this.source.origin(this.decl), generatedDeclaration: name, policy: 'transparent-native-statement-factoring-v1' })
      this.body.push(name)
    } else this.body.push(stmt)
  }
  fresh(hint = 'tmp') { return `v_${this.decl.id}_${this.serial++}_${hint.replace(/[^A-Za-z0-9_]/g, '_')}` }
  bind(expr, hint = 'tmp') { const name = this.fresh(hint); this.emit(`.letVar ${q(name)} ${expr}`); return e('localVar', q(name)) }
  panicUnless(cond, code) { this.emit(`.requireError ${cond} "Panic" [${lit(code)}]`) }
  abiRequire(cond) {
    if (this.owner.options.abiRequire) return this.owner.options.abiRequire(cond, this)
    this.fail('ABI malformed-data empty-revert hook not installed')
  }
  capture(fn) {
    const old = this.body, env = this.env; this.body = []; this.env = new Map(env)
    try { const result = fn(); return { body: this.body, result } } finally { this.body = old; this.env = env }
  }
  eval(node) { return this.value(this.expression(node)) }
  readDeclaration(id) {
    const d = this.source.declaration(id)
    return this.value(this.expression({ nodeType: 'Identifier', referencedDeclaration: id, id: d.id, src: d.src }))
  }
  writeDeclaration(id, v) {
    const target = this.env.get(id)
    if (!target) this.fail(`unbound assembly declaration ${id}`)
    if (target.place === 'scalar') return this.assign(target, v)
    if (target.place === 'memory' && target.local) { this.emit(`.assignVar ${q(target.local)} ${v.ptr}`); return }
    this.fail('assembly write needs mutable scalar/memory local')
  }
  memory(v) {
    const abi = this.owner.options.abi
    if (!abi) this.fail('ABI materialization hook not installed')
    if (v.place === 'memory') return v
    if (v.place === 'storage') {
      if (v.type.kind !== 'array' || v.type.length === null || v.type.element.kind !== 'scalar') this.fail('storage aggregate copy shape unsupported')
      const values = Array.from({ length: Number(v.type.length) }, (_, i) => this.freeze({ ...v, type: v.type.element, steps: [...v.steps, { kind: 'index', expr: lit(i) }] }))
      return this.arrayMemory(v.type, values)
    }
    if (!['calldata', 'literal'].includes(v.place)) this.fail(`cannot materialize ${v.place}`)
    const encoded = abi.encodeTuple([v], this), base = abi.byteData(encoded, this)
    return { ...v, place: 'memory', ptr: this.bind(abi.isDynamic(v.type) ? e('add', base, e('mload', base)) : base, 'memory_view') }
  }
  arrayMemory(type, values) {
    const abi = this.owner.options.abi
    if (!abi) this.fail('ABI inline array hook not installed')
    if (type.length === null || Number(type.length) !== values.length || abi.isDynamic(type.element)) this.fail('unsupported inline array ABI shape')
    const encoded = abi.encodeTuple(values.map(v => ({ ...this.value(v), type: type.element })), this)
    return { type, place: 'memory', ptr: abi.byteData(encoded, this) }
  }
  typeOf(node) {
    const id = node.typeDescriptions?.typeIdentifier ?? ''
    const m = /^t_(u?int)(\d+)/.exec(id)
    if (m) return { kind: 'scalar', abi: `${m[1]}${m[2]}`, bits: Number(m[2]), signed: m[1] === 'int' }
    if (id.startsWith('t_bool')) return bool
    if (id.startsWith('t_address') || id.startsWith('t_contract')) return { ...uint(160), abi: 'address' }
    const b = /^t_bytes(\d+)/.exec(id)
    if (b) return { ...uint(Number(b[1]) * 8), abi: `bytes${b[1]}`, alignment: 'left' }
    if (id.startsWith('t_rational')) return uint(256)
    if (node.typeName) return this.source.type(node)
    this.fail(`unsupported expression type ${id}`, node)
  }
  clean(expr, type) {
    if (type.kind !== 'scalar') this.fail('cleanup of composite')
    if (type.abi === 'bool') return e('logicalNot', e('logicalNot', expr))
    if (type.signed) return type.bits === 256 ? expr : e('signextend', lit(type.bits / 8 - 1), expr)
    if (type.alignment === 'left') return type.bits === 256 ? expr : e('bitAnd', expr, lit(((1n << BigInt(type.bits)) - 1n) << BigInt(256 - type.bits)))
    return type.bits === 256 ? expr : e('bitAnd', expr, lit((1n << BigInt(type.bits)) - 1n))
  }
  value(v) {
    if (v.place === 'storage' && v.type.kind === 'scalar') return scalar(v.type, this.readStorage(v))
    return v
  }
  word(v, node) { v = this.value(v); if (v.place !== 'scalar') this.fail(`expected scalar, got ${v.place}`, node); return v.expr }
  freeze(v) {
    v = this.value(v)
    if (v.place === 'tuple') return { ...v, values: v.values.map(x => x && this.freeze(x)) }
    if (v.place === 'scalar') return scalar(v.type, this.bind(v.expr, 'snapshot'))
    return v
  }
  storageCandidates(v) {
    const root = this.owner.storage.get(v.declaration)
    if (!root) this.fail('unknown storage root')
    const keys = v.steps.slice(0, root.keys.length).map(s => s.expr), tail = v.steps.slice(root.keys.length)
    const candidates = root.leaves.filter(l => l.steps.length === tail.length && l.steps.every((s, i) => s.kind === tail[i].kind && (s.kind !== 'member' || s.declaration === tail[i].declaration)))
    if (keys.some(k => !k) || !candidates.length || v.type.kind !== 'scalar') this.fail('non-scalar/incomplete storage path')
    return { root, keys, candidates, tail }
  }
  storageOp(root, keys, leaf, value) {
    if (!keys.length) return value === undefined ? e(root.type.abi === 'address' ? 'storageAddr' : 'storage', q(root.name)) : `.set${root.type.abi === 'address' ? 'StorageAddr' : 'Storage'} ${q(root.name)} ${value}`
    const args = [q(root.name), ...keys, q(leaf.name)]
    return value === undefined ? e(`structMember${keys.length === 2 ? '2' : ''}`, ...args) : `.setStructMember${keys.length === 2 ? '2' : ''} ${args.join(' ')} ${value}`
  }
  readStorage(v) {
    const { root, keys, candidates, tail } = this.storageCandidates(v)
    if (candidates.length === 1) return this.clean(this.storageOp(root, keys, candidates[0]), v.type)
    const name = this.fresh('storage_index'); this.emit(`.letVar ${q(name)} ${lit(0)}`)
    for (const leaf of candidates) {
      const cond = leaf.steps.reduce((c, s, i) => s.kind === 'index' ? e('logicalAnd', c, e('eq', tail[i].expr, lit(s.index))) : c, lit(1))
      this.emit(`.ite ${cond} [.assignVar ${q(name)} ${this.clean(this.storageOp(root, keys, leaf), v.type)}] []`)
    }
    return e('localVar', q(name))
  }
  assign(target, value) {
    if (target.place === 'tuple') {
      if (value.place !== 'tuple' || value.values.length !== target.values.length) this.fail('tuple assignment shape')
      value = this.freeze(value)
      target.values.forEach((v, i) => { if (v) this.assign(v, value.values[i]) }); return
    }
    const expr = this.clean(this.word(value), target.type)
    if (target.place === 'scalar' && target.local) { this.emit(`.assignVar ${q(target.local)} ${expr}`); return }
    if (target.place !== 'storage') this.fail(`unsupported lvalue ${target.place}`)
    const { root, keys, candidates, tail } = this.storageCandidates(target)
    if (root.immutable && this.decl.kind !== 'constructor') this.fail('immutable write outside constructor')
    if (candidates.length === 1) this.emit(this.storageOp(root, keys, candidates[0], expr))
    else for (const leaf of candidates) {
      const cond = leaf.steps.reduce((c, s, i) => s.kind === 'index' ? e('logicalAnd', c, e('eq', tail[i].expr, lit(s.index))) : c, lit(1))
      this.emit(`.ite ${cond} [${this.storageOp(root, keys, leaf, expr)}] []`)
    }
  }
  binary(op, a, b, type, node) {
    const left = this.bind(this.word(a), 'lhs'), right = this.bind(this.word(b), 'rhs')
    const signed = a.type?.signed || b.type?.signed
    const ops = { '+': 'add', '-': 'sub', '*': 'mul', '/': signed ? 'sdiv' : 'div', '%': signed ? 'smod' : 'mod', '&': 'bitAnd', '|': 'bitOr', '^': 'bitXor', '==': 'eq', '<': signed ? 'slt' : 'lt', '>': signed ? 'sgt' : 'gt' }
    if (op === '!=') return scalar(bool, e('logicalNot', e('eq', left, right)))
    if (op === '<=' || op === '>=') return scalar(bool, e('logicalNot', e(signed ? (op === '<=' ? 'sgt' : 'slt') : (op === '<=' ? 'gt' : 'lt'), left, right)))
    if (op === '<<' || op === '>>') return scalar(type, this.clean(e(op === '<<' ? 'shl' : signed ? 'sar' : 'shr', right, left), type))
    if (!ops[op]) this.fail(`binary operator ${op}`, node)
    if (['/', '%'].includes(op)) this.panicUnless(e('logicalNot', e('eq', right, lit(0))), 0x12)
    if (signed && ['+', '-', '*', '/'].includes(op)) {
      const raw = this.bind(e(ops[op], left, right), 'signed_result'), result = this.clean(raw, type)
      if (!this.unchecked) {
        const min = lit(-(1n << BigInt(type.bits - 1))), minusOne = lit(-1)
        if (op === '/' || op === '*') this.panicUnless(e('logicalNot', e('logicalAnd', e('eq', left, min), e('eq', right, minusOne))), 0x11)
        if (op === '+') this.panicUnless(e('logicalOr', e('slt', right, lit(0)), e('logicalNot', e('slt', raw, left))), 0x11)
        if (op === '+') this.panicUnless(e('logicalOr', e('logicalNot', e('slt', right, lit(0))), e('slt', raw, left)), 0x11)
        if (op === '-') this.panicUnless(e('logicalOr', e('slt', right, lit(0)), e('logicalNot', e('sgt', raw, left))), 0x11)
        if (op === '-') this.panicUnless(e('logicalOr', e('logicalNot', e('slt', right, lit(0))), e('sgt', raw, left)), 0x11)
        if (op === '*') this.panicUnless(e('logicalOr', e('eq', right, lit(0)), e('eq', e('sdiv', raw, right), left)), 0x11)
        this.panicUnless(e('eq', result, raw), 0x11)
      }
      return scalar(type, result)
    }
    if (!this.unchecked && ['+', '-', '*'].includes(op)) {
      const max = lit((1n << BigInt(type.bits)) - 1n)
      if (op === '+') this.panicUnless(e('le', left, e('sub', max, right)), 0x11)
      if (op === '-') this.panicUnless(e('ge', left, right), 0x11)
      if (op === '*') this.panicUnless(e('logicalOr', e('eq', right, lit(0)), e('le', left, e('div', max, right))), 0x11)
    }
    return scalar(type, this.clean(e(ops[op], left, right), type))
  }
  expression(node) {
    if (!node) this.fail('missing expression')
    switch (node.nodeType) {
      case 'Literal': {
        if (node.kind === 'bool') return scalar(bool, lit(node.value === 'true' ? 1 : 0))
        if (node.kind === 'number') return scalar(this.typeOf(node), lit(this.source.constant(node)))
        if (['string', 'unicodeString', 'hexString'].includes(node.kind)) return { place: 'literal', type: { kind: 'bytes', abi: 'bytes' }, hex: node.hexValue }
        break
      }
      case 'Identifier': {
        if (node.referencedDeclaration < 0 && node.name === 'this') return scalar(this.typeOf(node), e('contractAddress'))
        const d = this.source.resolve(node)
        if (d.constant) return this.source.type(d).kind === 'scalar' ? scalar(this.source.type(d), lit(this.source.constant(d.value))) : { ...this.expression(d.value), type: this.source.type(d) }
        if (d.stateVariable) return { place: 'storage', type: this.source.type(d), declaration: d.id, steps: [] }
        if (this.env.has(d.id)) return this.env.get(d.id)
        this.fail(`unbound declaration ${d.id}`, node)
        break
      }
      case 'MemberAccess': {
        const hooked = this.owner.options.member?.(node, this)
        if (hooked !== undefined) return hooked
        if (node.expression.nodeType === 'Identifier' && node.expression.referencedDeclaration < 0) {
          const op = { 'msg.sender': 'caller', 'msg.value': 'msgValue', 'block.timestamp': 'blockTimestamp', 'block.chainid': 'chainid', 'block.number': 'blockNumber', 'tx.origin': 'txOrigin' }[`${node.expression.name}.${node.memberName}`]
          if (op) return scalar(this.typeOf(node), e(op))
        }
        if (node.referencedDeclaration >= 0 && this.source.resolve(node).constant) return scalar(this.typeOf(node), lit(this.source.constant(node)))
        if (['max', 'min'].includes(node.memberName)) return scalar(this.typeOf(node), lit(this.source.constant(node)))
        const base = this.expression(node.expression)
        if (node.memberName === 'length') {
          if (base.type.kind === 'array' && base.type.length !== null) return scalar(uint(256), lit(base.type.length))
          if (['memory', 'calldata'].includes(base.place)) return scalar(uint(256), e(base.place === 'memory' ? 'mload' : 'calldataload', base.ptr))
          this.fail('length on unsupported location', node)
        }
        const member = this.source.resolve(node, 'VariableDeclaration'), type = this.source.type(member)
        if (base.place === 'storage') return { ...base, type, steps: [...base.steps, { kind: 'member', declaration: member.id }] }
        if (this.owner.options.abi) return this.owner.options.abi.readMember(base, member.id, this)
        this.fail('ABI member hook not installed', node)
        break
      }
      case 'IndexAccess': {
        const literalArray = node.baseExpression
        if (literalArray.nodeType === 'TupleExpression' && literalArray.isInlineArray && this.typeOf(node).kind === 'scalar') {
          // A source scalar array literal indexed immediately needs no heap.
          // Preserve element evaluation and the Solidity bounds panic, while
          // retaining pure source functions as pure in the backend model.
          const values = literalArray.components.map(component => this.word(this.freeze(this.expression(component))))
          const index = this.bind(this.word(this.expression(node.indexExpression)), 'index')
          this.panicUnless(e('lt', index, lit(values.length)), 0x32)
          let selected = lit(0)
          for (let i = values.length - 1; i >= 0; i--) selected = e('ite', e('eq', index, lit(i)), values[i], selected)
          return scalar(this.typeOf(node), selected)
        }
        const base = this.expression(node.baseExpression), index = this.bind(this.word(this.expression(node.indexExpression)), 'index')
        if (base.place === 'storage') {
          if (base.type.kind === 'array') {
            if (base.type.length === null) this.fail('dynamic storage array', node)
            this.panicUnless(e('lt', index, lit(base.type.length)), 0x32)
          }
          const type = base.type.kind === 'mapping' ? base.type.value : base.type.element
          return { ...base, type, steps: [...base.steps, { kind: 'index', expr: index }] }
        }
        if (base.place === 'inlineArray') {
          this.panicUnless(e('lt', index, lit(base.values.length)), 0x32)
          const name = this.fresh('array_index'); this.emit(`.letVar ${q(name)} ${lit(0)}`)
          base.values.forEach((v, i) => this.emit(`.ite ${e('eq', index, lit(i))} [.assignVar ${q(name)} ${this.word(v)}] []`))
          return scalar(base.type.element, e('localVar', q(name)))
        }
        if (this.owner.options.abi) return this.owner.options.abi.readIndex(base, index, this)
        this.fail('ABI index hook not installed', node)
        break
      }
      case 'TupleExpression':
        if (node.isInlineArray) {
          const values = node.components.map(n => this.freeze(this.expression(n)))
          if (!values.length || values.some(v => v.place !== 'scalar')) this.fail('composite inline array allocation not installed', node)
          return this.arrayMemory({ kind: 'array', element: values[0].type, length: String(values.length) }, values)
        }
        if (node.components.length === 1 && node.components[0]) return this.expression(node.components[0])
        return { place: 'tuple', values: node.components.map(n => n && this.expression(n)) }
      case 'Assignment': {
        // Snapshot RHS before writes; tuple RHS is simultaneous, not sequential assignment.
        const rhs = this.freeze(this.expression(node.rightHandSide)), lhs = this.expression(node.leftHandSide)
        const value = node.operator === '=' ? rhs : this.binary(node.operator.slice(0, -1), this.value(lhs), rhs, lhs.type, node)
        this.assign(lhs, value); return value
      }
      case 'BinaryOperation': {
        if (['&&', '||'].includes(node.operator)) {
          const left = this.word(this.expression(node.leftExpression)), name = this.fresh('lazy')
          this.emit(`.letVar ${q(name)} ${left}`)
          const branch = this.capture(() => this.emit(`.assignVar ${q(name)} ${this.word(this.expression(node.rightExpression))}`))
          this.emit(`.ite ${node.operator === '&&' ? e('localVar', q(name)) : e('logicalNot', e('localVar', q(name)))} ${list(branch.body)} []`)
          return scalar(bool, e('localVar', q(name)))
        }
        const a = this.freeze(this.expression(node.leftExpression)), b = this.expression(node.rightExpression)
        return this.binary(node.operator, a, b, this.typeOf(node), node)
      }
      case 'Conditional': {
        const cond = this.word(this.expression(node.condition))
        const yes = this.capture(() => this.freeze(this.expression(node.trueExpression)))
        const no = this.capture(() => this.freeze(this.expression(node.falseExpression)))
        const merge = (a, b) => {
          if (a.place === 'tuple' && b.place === 'tuple' && a.values.length === b.values.length) return { place: 'tuple', values: a.values.map((v, i) => merge(v, b.values[i])) }
          if (a.place !== 'scalar' || b.place !== 'scalar') {
            if (!['calldata', 'memory'].includes(a.place) || a.place !== b.place || this.source.abiType(a.type) !== this.source.abiType(b.type)) this.fail('composite conditional incompatible locations/types', node)
            const name = this.fresh('phi_view'); this.emit(`.letVar ${q(name)} ${lit(0)}`)
            yes.body.push(`.assignVar ${q(name)} ${a.ptr}`); no.body.push(`.assignVar ${q(name)} ${b.ptr}`)
            return { type: a.type, place: a.place, ptr: e('localVar', q(name)) }
          }
          const name = this.fresh('phi'); this.emit(`.letVar ${q(name)} ${lit(0)}`)
          yes.body.push(`.assignVar ${q(name)} ${a.expr}`); no.body.push(`.assignVar ${q(name)} ${b.expr}`)
          return scalar(a.type, e('localVar', q(name)))
        }
        const value = merge(yes.result, no.result)
        this.emit(`.ite ${cond} ${list(yes.body)} ${list(no.body)}`); return value
      }
      case 'UnaryOperation': {
        const target = this.expression(node.subExpression)
        if (['++', '--'].includes(node.operator)) {
          const before = this.freeze(target), after = this.binary(node.operator === '++' ? '+' : '-', before, scalar(target.type, lit(1)), target.type, node)
          this.assign(target, after); return node.prefix ? after : before
        }
        if (node.operator === 'delete') { this.assign(target, scalar(target.type, lit(0))); return scalar(target.type, lit(0)) }
        const word = this.word(target)
        if (node.operator === '!') return scalar(bool, e('logicalNot', word))
        if (node.operator === '~') return scalar(target.type, this.clean(e('bitNot', word), target.type))
        if (node.operator === '-') {
          // Rational literals have no runtime operand width; evaluate exactly first.
          if (node.typeDescriptions?.typeIdentifier?.startsWith('t_rational')) return scalar(uint(256), lit(this.source.constant(node)))
          if (!target.type.signed) this.fail('runtime unsigned negation', node)
          const x = this.bind(word, 'negate')
          if (!this.unchecked) this.panicUnless(e('logicalNot', e('eq', x, lit(-(1n << BigInt(target.type.bits - 1))))), 0x11)
          return scalar(target.type, this.clean(e('sub', lit(0), x), target.type))
        }
        break
      }
      case 'FunctionCall': return this.call(node)
    }
    this.fail(`unsupported expression ${node.nodeType}`, node)
  }
  call(node) {
    const callee = node.expression
    if (node.kind === 'typeConversion') {
      if (node.arguments.length !== 1) this.fail('conversion arity', node)
      const type = callee.typeName ? this.source.type(callee.typeName) : this.typeOf(node)
      const input = this.value(this.expression(node.arguments[0]))
      let word = this.word(input)
      if (input.type.alignment !== type.alignment && (input.type.alignment || type.alignment)) {
        if (input.type.bits !== type.bits) this.fail('cross-width fixed-bytes/integer conversion', node)
        word = e(type.alignment === 'left' ? 'shl' : 'shr', lit(256 - type.bits), word)
      }
      return scalar(type, this.clean(word, type))
    }
    if (callee.nodeType === 'Identifier' && callee.referencedDeclaration < 0 && ['require', 'assert'].includes(callee.name)) {
      const cond = this.word(this.expression(node.arguments[0]))
      if (callee.name === 'assert') this.panicUnless(cond, 1)
      else {
        const err = node.arguments[1]
        if (err?.nodeType !== 'FunctionCall' || err.expression.referencedDeclaration < 0) this.fail('require needs typed custom error (string/empty not installed)', node)
        const decl = this.source.resolve(err.expression, 'ErrorDefinition')
        if (err.arguments.length) this.fail('lazy parameterized require error not installed', node)
        this.emit(`.requireError ${cond} ${q(decl.name)} []`)
      }
      return { place: 'tuple', values: [] }
    }
    if (callee.referencedDeclaration >= 0) {
      const decl = this.source.resolve(callee)
      if (decl.nodeType === 'FunctionDefinition' && decl.body) {
        const bound = callee.nodeType === 'MemberAccess' && (callee.typeDescriptions?.typeIdentifier?.includes('bound_to') || callee.typeDescriptions?.typeIdentifier?.includes('attached_to') && decl.parameters.parameters.length === node.arguments.length + 1)
        if (callee.nodeType === 'MemberAccess' && !bound) {
          // Library-qualified static calls are permitted; instance calls are external.
          const receiver = callee.expression.referencedDeclaration >= 0 ? this.source.resolve(callee.expression) : null
          if (receiver?.nodeType !== 'ContractDefinition' || receiver.contractKind !== 'library') {
            const hooked = this.owner.options.call?.(node, this)
            if (hooked !== undefined) return hooked
            this.fail('external call boundary not installed', node)
          }
        }
        const args = []
        if (bound) args.push(this.freeze(this.expression(callee.expression)))
        args.push(...node.arguments.map(a => this.freeze(this.expression(a))))
        if (args.length !== decl.parameters.parameters.length) this.fail('internal call arity', node)
        if (decl.parameters.parameters.some(p => p.storageLocation === 'storage')) this.fail('internal storage-reference calling convention not installed', node)
        const name = this.owner.functionName(decl, true), words = list(args.map((v, i) => this.source.type(decl.parameters.parameters[i]).kind === 'scalar' ? this.clean(this.word(v), this.source.type(decl.parameters.parameters[i])) : this.memory(v).ptr)), returns = decl.returnParameters.parameters
        if (!returns.length) { this.emit(`.internalCall ${q(name)} ${words}`); return { place: 'tuple', values: [] } }
        if (returns.some(p => this.source.type(p).kind !== 'scalar')) this.fail('internal composite return not installed', node)
        if (returns.length === 1) {
          const result = this.bind(e('internalCall', q(name), words), 'call')
          return scalar(this.source.type(returns[0]), result)
        }
        const names = returns.map(() => this.fresh('call'))
        this.emit(`.internalCallAssign ${list(names.map(q))} ${q(name)} ${words}`)
        const values = returns.map((p, i) => scalar(this.source.type(p), e('localVar', q(names[i]))))
        return values.length === 1 ? values[0] : { place: 'tuple', values }
      }
    }
    const hooked = this.owner.options.call?.(node, this)
    if (hooked !== undefined) return hooked
    this.fail('unimplemented builtin/external/assembly call boundary', node)
  }
  bitmapBound(node) {
    const cond = node.condition, ss = node.body?.statements
    if (cond?.operator !== '!=' || cond.leftExpression?.nodeType !== 'Identifier' || cond.rightExpression?.kind !== 'number' || this.source.constant(cond.rightExpression) !== 0n || !ss?.length) this.fail('unsupported while measure', node)
    const id = cond.leftExpression.referencedDeclaration, type = this.source.type(this.source.declaration(id))
    if (type.kind !== 'scalar' || type.signed || !/^uint/.test(type.abi)) this.fail('while measure must be unsigned bitmap', node)
    const first = ss[0], last = ss.at(-1)?.expression, pick = first?.initialValue
    if (first?.nodeType !== 'VariableDeclarationStatement' || first.declarations.length !== 1 || pick?.nodeType !== 'FunctionCall' || pick.arguments.length !== 1 || pick.arguments[0].referencedDeclaration !== id || last?.nodeType !== 'Assignment' || last.operator !== '=' || last.leftHandSide.referencedDeclaration !== id) this.fail('while lacks unconditional pick/clear structure', node)
    const bitId = first.declarations[0].id, clear = last.rightHandSide
    if (clear.nodeType !== 'FunctionCall' || clear.expression.nodeType !== 'MemberAccess' || clear.expression.expression.referencedDeclaration !== id || clear.arguments.length !== 1 || clear.arguments[0].referencedDeclaration !== bitId) this.fail('while clear does not use picked bit', node)
    const msb = this.source.resolve(pick.expression, 'FunctionDefinition'), clr = this.source.resolve(clear.expression, 'FunctionDefinition')
    const mp = msb.parameters.parameters, cp = clr.parameters.parameters
    if (mp.length !== 1 || cp.length !== 2 || this.source.type(mp[0]).bits !== type.bits || this.source.type(cp[0]).bits !== type.bits) this.fail('bitmap helper widths disagree', node)
    const indexTypes = [this.source.type(first.declarations[0]), ...msb.returnParameters.parameters.map(p => this.source.type(p)), this.source.type(cp[1])]
    if (msb.returnParameters.parameters.length !== 1 || indexTypes.some(t => t.kind !== 'scalar' || t.signed || !/^uint/.test(t.abi) || (1n << BigInt(t.bits)) < BigInt(type.bits))) this.fail('bitmap picked-bit types cannot represent full index range', node)
    // Validate the executable helper ASTs, not the msb/clearBit display names.
    const assembly = msb.body?.statements
    const y = assembly?.[0]?.AST?.statements?.[0], sub = y?.value
    const isY = (n, op, arity) => n?.nodeType === 'YulFunctionCall' && n.functionName.name === op && n.arguments.length === arity
    const refs = assembly?.[0]?.externalReferences ?? []
    if (assembly?.length !== 1 || assembly[0].AST?.statements?.length !== 1 || y?.nodeType !== 'YulAssignment' || y.variableNames.length !== 1 || !isY(sub, 'sub', 2) || sub.arguments[0].value !== '255' || !isY(sub.arguments[1], 'clz', 1) || refs.find(r => r.src === sub.arguments[1].arguments[0].src)?.declaration !== mp[0].id || refs.find(r => r.src === y.variableNames[0].src)?.declaration !== msb.returnParameters.parameters[0]?.id) this.fail('while pick helper not structurally 255-clz(bitmap)', node)
    const ret = clr.body?.statements?.[0], cast = ret?.expression, and = cast?.arguments?.[0], not = and?.rightExpression, paren = not?.subExpression, shift = paren?.nodeType === 'TupleExpression' ? paren.components[0] : paren
    if (clr.body?.statements?.length !== 1 || ret.nodeType !== 'Return' || cast?.kind !== 'typeConversion' || this.typeOf(cast).bits !== type.bits || and?.operator !== '&' || and.leftExpression.referencedDeclaration !== cp[0].id || not?.operator !== '~' || shift?.operator !== '<<' || shift.leftExpression.kind !== 'number' || this.source.constant(shift.leftExpression) !== 1n || shift.rightExpression.referencedDeclaration !== cp[1].id) this.fail('while clear helper not structurally bitmap & ~(1 << bit)', node)
    let writes = 0
    walk(node.body, n => {
      const lvalueIDs = lhs => {
        if (!lhs) return []
        if (lhs.nodeType === 'TupleExpression') return lhs.components.flatMap(lvalueIDs)
        if (lhs.nodeType === 'IndexAccess' || lhs.nodeType === 'IndexRangeAccess') return lvalueIDs(lhs.baseExpression)
        if (lhs.nodeType === 'MemberAccess') return lvalueIDs(lhs.expression)
        return lhs.referencedDeclaration === undefined ? [] : [lhs.referencedDeclaration]
      }
      if (n.nodeType === 'Assignment') {
        const ids = lvalueIDs(n.leftHandSide)
        writes += ids.filter(x => x === id).length
        if (ids.includes(bitId)) this.fail('while selected bit mutation', n)
      }
      if (n.nodeType === 'UnaryOperation' && ['++', '--', 'delete'].includes(n.operator) && lvalueIDs(n.subExpression).some(x => x === id || x === bitId)) this.fail('while measure mutation', n)
      if (['Continue', 'Break', 'InlineAssembly', 'WhileStatement', 'ForStatement'].includes(n.nodeType)) this.fail('unsupported control/effect inside bitmap measure loop', n)
    })
    if (writes !== 1) this.fail('while measure has additional writes', node)
    return type.bits
  }
  statements(node) {
    if (!node) return
    switch (node.nodeType) {
      case 'Block': case 'UncheckedBlock': {
        const old = this.unchecked, env = this.env; this.env = new Map(env)
        if (node.nodeType === 'UncheckedBlock') this.unchecked = true
        try { node.statements.forEach(s => this.statements(s)) } finally { this.unchecked = old; this.env = env }
        return
      }
      case 'ExpressionStatement': this.expression(node.expression); return
      case 'VariableDeclarationStatement': {
        const value = node.initialValue ? this.expression(node.initialValue) : null
        const values = value?.place === 'tuple' ? value.values : [value]
        if (node.declarations.length !== values.length) this.fail('declaration tuple shape', node)
        const snapshots = values.map(v => v && (v.place === 'storage' ? v : this.freeze(v)))
        node.declarations.forEach((decl, i) => {
          if (!decl) return
          const type = this.source.type(decl), v = snapshots[i]
          if (decl.storageLocation === 'storage') {
            if (v?.place !== 'storage') this.fail('storage alias requires storage value', decl)
            this.env.set(decl.id, v); return
          }
          if (type.kind !== 'scalar') {
            if (!v) this.fail('composite local initialization not installed', decl)
            const view = decl.storageLocation === 'calldata' ? v : this.memory(v)
            if (!['calldata', 'memory'].includes(view.place)) this.fail('invalid composite local location', decl)
            const local = `local_${decl.id}`
            this.emit(`.letVar ${q(local)} ${view.ptr}`)
            this.env.set(decl.id, { ...view, type, ptr: e('localVar', q(local)), local }); return
          }
          const name = `local_${decl.id}`
          this.emit(`.letVar ${q(name)} ${v ? this.clean(this.word(v), type) : lit(0)}`)
          this.env.set(decl.id, { ...scalar(type, e('localVar', q(name))), local: name })
        }); return
      }
      case 'WhileStatement': {
        const bound = this.bitmapBound(node), name = this.fresh('bitmap_iteration')
        const iteration = this.capture(() => {
          const cond = this.word(this.expression(node.condition)), body = this.capture(() => this.statements(node.body))
          this.emit(`.ite ${cond} ${list(body.body)} []`)
        })
        this.emit(`.forEach ${q(name)} ${lit(bound)} ${list(iteration.body)}`); return
      }
      case 'ForStatement': {
        const init = node.initializationExpression, cond = node.condition, step = node.loopExpression?.expression
        const d = init?.declarations?.[0], id = d?.id
        if (init?.nodeType !== 'VariableDeclarationStatement' || init.declarations.length !== 1 || init.initialValue?.kind !== 'number' || this.source.constant(init.initialValue) !== 0n || cond?.operator !== '<' || cond.leftExpression?.referencedDeclaration !== id || step?.nodeType !== 'UnaryOperation' || step.operator !== '++' || step.subExpression?.referencedDeclaration !== id || cond.rightExpression?.nodeType !== 'MemberAccess' || cond.rightExpression.memberName !== 'length') this.fail('unsupported for-loop induction shape', node)
        const array = this.expression(cond.rightExpression.expression)
        if (array.type.kind !== 'array' || !['memory', 'calldata'].includes(array.place)) this.fail('for bound must be readonly ABI array length', node)
        const dependencies = new Set()
        walk(cond.rightExpression, n => { if (n.nodeType === 'Identifier') dependencies.add(n.referencedDeclaration) })
        walk(node.body, n => {
          if (n.nodeType === 'Assignment') { let bad = false; walk(n.leftHandSide, x => { if (x.nodeType === 'Identifier' && (x.referencedDeclaration === id || dependencies.has(x.referencedDeclaration))) bad = true }); if (bad) this.fail('for loop modifies bound/counter', n) }
          if (n.nodeType === 'UnaryOperation' && ['++', '--', 'delete'].includes(n.operator) && (n.subExpression?.referencedDeclaration === id || dependencies.has(n.subExpression?.referencedDeclaration))) this.fail('for loop mutates bound/counter', n)
          if (['Break', 'Continue'].includes(n.nodeType)) this.fail('for control unsupported', n)
          if (n.nodeType === 'InlineAssembly' && n.externalReferences?.some(r => r.declaration === id || dependencies.has(r.declaration))) this.fail('assembly may mutate for bound/counter', n)
        })
        this.statements(init)
        const count = this.bind(this.owner.options.abi.arrayLength(array, this), 'for_bound'), name = this.fresh('source_iteration')
        const body = this.capture(() => { this.statements(node.body); this.statements(node.loopExpression) })
        this.emit(`.forEach ${q(name)} ${count} ${list(body.body)}`); return
      }
      case 'IfStatement': {
        const cond = this.word(this.expression(node.condition))
        const yes = this.capture(() => this.statements(node.trueBody)), no = this.capture(() => this.statements(node.falseBody))
        this.emit(`.ite ${cond} ${list(yes.body)} ${list(no.body)}`); return
      }
      case 'Return': {
        let value = node.expression ? this.expression(node.expression) : { place: 'tuple', values: (this.decl.returnParameters?.parameters ?? []).map(p => this.env.get(p.id)) }
        const values = value.place === 'tuple' ? value.values : [value]
        const returns = this.decl.returnParameters.parameters
        if (values.length !== returns.length) this.fail('return arity', node)
        if (returns.some(p => this.source.type(p).kind !== 'scalar')) {
          if (this.internal || !['public', 'external'].includes(this.decl.visibility)) this.fail('internal composite return unsupported', node)
          const abi = this.owner.options.abi
          if (!abi) this.fail('composite return ABI hook not installed', node)
          const encoded = abi.encodeTuple(values.map((v, i) => ({ ...this.value(v), type: this.source.type(returns[i]) })), this)
          const offset = this.fresh('return_offset'), size = this.fresh('return_size')
          this.emit(`.letVar ${q(offset)} ${abi.byteData(encoded, this)}`)
          this.emit(`.letVar ${q(size)} ${abi.byteLength(encoded, this)}`)
          const boundary = this.fresh('source_abi_return')
          // RETURN is terminal, not an external call. Keep the memory-slice
          // obligation explicit; do not exempt this function from return checks.
          this.emit(`.unsafeYul { label := ${q(boundary)}, stmts := [.expr (.call "return" [.ident ${q(offset)}, .ident ${q(size)}])], obligations := [{ name := ${q(boundary)}, obligation := "The source-derived ABI encoder supplies the exact return memory slice; EVM RETURN terminates this call with those bytes.", proofStatus := .assumed }], termination := .alwaysTerminates, controlFlow := .returns }`)
          return
        }
        this.emit(`.returnValues ${list(values.map((v, i) => this.clean(this.word(v, node), this.source.type(returns[i]))))}`); return
      }
      case 'EmitStatement': {
        const decl = this.source.resolve(node.eventCall.expression, 'EventDefinition')
        if (decl.parameters.parameters.some(p => this.source.type(p).kind !== 'scalar')) {
          const abi = this.owner.options.abi
          if (!abi) this.fail('composite event encoding hook not installed', node)
          const topics = [lit(BigInt('0x' + keccak256(Buffer.from(`${decl.name}(${decl.parameters.parameters.map(p => this.source.abiType(this.source.type(p))).join(',')})`))))], data = []
          node.eventCall.arguments.forEach((a, i) => {
            const p = decl.parameters.parameters[i], v = { ...this.freeze(this.expression(a)), type: this.source.type(p) }
            if (p.indexed) { if (v.type.kind !== 'scalar') this.fail('indexed composite event unsupported', node); topics.push(this.clean(this.word(v), v.type)) }
            else data.push(v)
          })
          const encoded = abi.encodeTuple(data, this)
          this.emit(`.rawLog ${list(topics)} ${abi.byteData(encoded, this)} ${abi.byteLength(encoded, this)}`); return
        }
        this.emit(`.emit ${q(decl.name)} ${list(node.eventCall.arguments.map(a => this.word(this.freeze(this.expression(a)))))}`); return
      }
      case 'RevertStatement': {
        const decl = this.source.resolve(node.errorCall.expression, 'ErrorDefinition')
        this.emit(`.revertError ${q(decl.name)} ${list(node.errorCall.arguments.map(a => this.word(this.freeze(this.expression(a)))))}`); return
      }
      default:
        if (this.owner.options.statement && this.owner.options.statement(node, this) === true) return
        this.fail(`unsupported statement ${node.nodeType}`, node)
    }
  }
  compile() {
    const decl = this.decl
    if (!decl.body) this.fail('function body missing')
    if (decl.modifiers?.length) this.fail('modifier expansion not installed')
    const helper = this.internal || !['public', 'external'].includes(decl.visibility) && decl.kind !== 'constructor'
    if (!helper) this.emit(`.mstore ${lit(64)} ${lit(128)}`)
    // Public heads start after the selector. Static composites occupy their full
    // inline head, while every dynamic shape occupies one offset word.
    let publicHeadOffset = 4
    const params = decl.parameters.parameters.map((p, i) => {
      const type = this.source.type(p), name = `arg_${p.id}`
      const headOffset = publicHeadOffset
      if (!helper && type.kind === 'scalar') publicHeadOffset += 32
      if (p.storageLocation === 'storage') this.fail('storage parameter calling convention not installed', p)
      if (type.kind !== 'scalar') {
        if (!this.owner.options.abi || decl.kind === 'constructor') this.fail('composite parameter calling convention not installed', p)
        const local = `local_${p.id}`
        const abi = this.owner.options.abi
        if (!helper) publicHeadOffset += abi.headBytes(type)
        // Pinned ParamLoading defines no scalar base local for public tuples
        // and dynamic composites. Its data offset skips a length word only for
        // bytes/string/T[]; dynamic tuples and fixed arrays point at their head.
        // Static composite aliases (where present) are values, never pointers.
        const dynamic = !helper && abi.isDynamic(type)
        // This pin has no Expr.paramDynamicDataOffset. Re-read the checked
        // top-level offset word instead: 4 + offset is the length pointer for
        // length-prefixed shapes and the head pointer for every other shape.
        // emit lifts this calldata load through its normal unsafe binding scope.
        const pointer = helper ? e('param', q(name)) : dynamic
          ? e('add', lit(4), e('calldataload', lit(headOffset)))
          : lit(headOffset)
        this.emit(`.letVar ${q(local)} ${pointer}`)
        this.env.set(p.id, { type, place: helper ? 'memory' : 'calldata', ptr: e('localVar', q(local)), local })
        return `{ name := ${q(name)}, ty := ${helper ? '.uint256' : this.owner.paramType(type)} }`
      }
      const local = `local_${p.id}`
      this.emit(`.letVar ${q(local)} ${this.clean(decl.kind === 'constructor' ? e('constructorArg', String(i)) : e('param', q(name)), type)}`)
      this.env.set(p.id, { ...scalar(type, e('localVar', q(local))), local })
      return `{ name := ${q(name)}, ty := ${this.owner.paramType(type)} }`
    })
    for (const p of decl.returnParameters.parameters) if (p.name) {
      const type = this.source.type(p), local = `local_${p.id}`
      if (type.kind !== 'scalar') this.fail('named composite return not installed', p)
      this.emit(`.letVar ${q(local)} ${lit(0)}`); this.env.set(p.id, { ...scalar(type, e('localVar', q(local))), local })
    }
    if (decl.kind === 'constructor') {
      for (const d of this.source.declarations().filter(d => d.nodeType === 'VariableDeclaration' && d.stateVariable && !d.constant && d.value)) this.assign({ place: 'storage', declaration: d.id, steps: [], type: this.source.type(d) }, this.expression(d.value))
    }
    this.statements(decl.body)
    if (decl.kind !== 'constructor' && decl.returnParameters.parameters.length && decl.returnParameters.parameters.every(p => p.name)) this.emit(`.returnValues ${list(decl.returnParameters.parameters.map(p => this.word(this.env.get(p.id))))}`)
    const common = `params := ${list(params)}, isPayable := ${decl.stateMutability === 'payable'}, body := ${list(this.body)}`
    if (decl.kind === 'constructor') return `{ ${common} }`
    const sourceMutability = decl.stateMutability
    const reasons = [...this.backendPurityReasons].sort()
    const modelMutability = sourceMutability === 'pure' && reasons.length ? 'view' : sourceMutability
    this.owner.functionMutability.set(this.owner.functionName(decl, this.internal), {
      sourceMutability, modelMutability,
      policy: modelMutability !== sourceMutability ? 'source-pure-backend-conservative-view-v1' : 'source-mutability-preserved-v1',
      reasons,
      claim: 'Source ABI mutability is authoritative and unchanged. Model view retains backend state-write validation; this boundary is NOT a proof of Solidity purity or equivalence.',
    })
    return `{ name := ${q(this.owner.functionName(decl, this.internal))}, ${common}, returnType := none, returns := ${list(decl.returnParameters.parameters.map(p => this.owner.paramType(this.source.type(p))))}, isInternal := ${!!this.internal || !['public', 'external'].includes(decl.visibility)}, isView := ${modelMutability === 'view'}, isPure := ${modelMutability === 'pure'}, allowPostInteractionWrites := ${this.owner.ceiPolicies.get(decl.id)?.allowPostInteractionWrites ?? false} }`
  }
}
