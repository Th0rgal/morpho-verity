import { keccak256 } from './keccak256.mjs'
import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { readFileSync, readdirSync } from 'node:fs'
import { resolve, relative } from 'node:path'

export const sha256 = bytes => createHash('sha256').update(bytes).digest('hex')
export const stableJson = value => JSON.stringify(value, function (key, item) {
  return item && !Array.isArray(item) && typeof item === 'object'
    ? Object.fromEntries(Object.keys(item).sort().map(k => [k, item[k]])) : item
}, 2) + '\n'
export const sortedEntries = object => Object.entries(object).sort(([a], [b]) => a < b ? -1 : a > b ? 1 : 0)
export function walk(node, visit) {
  if (!node || typeof node !== 'object') return
  if (node.nodeType) visit(node)
  for (const item of Object.values(node)) {
    if (Array.isArray(item)) item.forEach(n => walk(n, visit))
    else if (item && typeof item === 'object') walk(item, visit)
  }
}
export class SourceError extends Error {
  constructor(message, node) {
    super(`${message}; nodeType=${node?.nodeType ?? '?'}; id=${node?.id ?? '?'}; src=${node?.src ?? '?'}`)
    this.name = 'SourceError'
  }
}
const fail = (message, node) => { throw new SourceError(message, node) }
const assert = (condition, message, node) => { if (!condition) fail(message, node) }

/** Compile only typed AST, ABI, selectors, and storage layout. Never request IR/bytecode.
 * Pins apply to the executable bytes and clean upstream src, not just --version output.
 * No downloads or mutations occur here; provision the pinned solc in the existing cache.
 */
export function compilePinnedSource(root, config) {
  const sourceRoot = resolve(root, config.sourceRoot)
  const run = (command, args, options = {}) => {
    const result = spawnSync(command, args, { encoding: 'utf8', maxBuffer: 100_000_000, ...options })
    if (result.error || result.status !== 0) throw new Error(`${command}: ${result.error ?? result.stderr ?? result.stdout}`)
    return result.stdout.trim()
  }
  const commit = run('git', ['-C', sourceRoot, 'rev-parse', 'HEAD'])
  assert(commit === config.midnightCommit, `source commit drift: ${commit}`)
  assert(run('git', ['-C', sourceRoot, 'status', '--porcelain', '--untracked-files=all', '--', 'src']) === '', 'source tree drift')
  const solc = process.env.MORPHO_SOLC_0_8_34 || resolve(root, `.cache/solc-${config.solc.version}`)
  assert(sha256(readFileSync(solc)) === config.solc.sha256, 'solc binary checksum drift')
  assert(run(solc, ['--version']).includes(config.solc.version), 'solc version drift')
  const sources = {}
  const collect = dir => {
    for (const entry of readdirSync(dir, { withFileTypes: true }).sort((a, b) => a.name < b.name ? -1 : a.name > b.name ? 1 : 0)) {
      const path = resolve(dir, entry.name)
      if (entry.isDirectory()) collect(path)
      else if (entry.isFile() && entry.name.endsWith('.sol')) sources[relative(sourceRoot, path).replaceAll('\\', '/')] = { content: readFileSync(path, 'utf8') }
      else if (entry.isSymbolicLink()) fail(`symlink in source tree: ${path}`)
    }
  }
  collect(resolve(sourceRoot, 'src'))
  // git status alone is not a pin check: ignored source files and tracked files
  // marked assume-unchanged/skip-worktree can both be invisible to it. Compare
  // the actual compiler input set and bytes to immutable objects in the pin.
  const pinnedSources = new Map()
  for (const record of run('git', ['-C', sourceRoot, 'ls-tree', '-r', '-z', commit, '--', 'src']).split('\0').filter(Boolean)) {
    const separator = record.indexOf('\t')
    assert(separator >= 0, 'invalid pinned tree record')
    const path = record.slice(separator + 1)
    if (!path.endsWith('.sol')) continue
    const header = /^([0-7]{6}) blob ([0-9a-f]+)$/.exec(record.slice(0, separator))
    assert(header && ['100644', '100755'].includes(header[1]), `unsupported pinned source object ${path}`)
    assert(!pinnedSources.has(path), `duplicate pinned source ${path}`)
    pinnedSources.set(path, header[2])
  }
  assert(stableJson([...pinnedSources.keys()].sort()) === stableJson(Object.keys(sources).sort()), 'compiler source set differs from pinned tree')
  for (const [path, objectId] of pinnedSources) {
    const actualObjectId = run('git', ['-C', sourceRoot, 'hash-object', '--stdin'], { input: sources[path].content })
    assert(actualObjectId === objectId, `source bytes differ from pinned tree: ${path}`)
  }
  const input = { language: 'Solidity', sources, settings: {
    optimizer: { enabled: true, runs: 800 }, viaIR: true, evmVersion: 'osaka',
    metadata: { bytecodeHash: 'none' },
    outputSelection: { '*': { '': ['ast'], '*': ['storageLayout', 'abi', 'evm.methodIdentifiers'] } },
  } }
  const output = JSON.parse(run(solc, ['--standard-json', '--base-path', sourceRoot, '--allow-paths', sourceRoot], { input: JSON.stringify(input) }))
  const errors = (output.errors ?? []).filter(e => e.severity === 'error')
  assert(errors.length === 0, errors.map(e => e.formattedMessage).join('\n'))
  // Reject unexpected compiler-loaded imports: every origin needs pinned source bytes.
  for (const file of Object.keys(output.sources ?? {})) assert(sources[file], `uncollected source ${file}`)
  return { input, output, commit, compiler: config.solc }
}

/** ID-resolved typed source and exact physical storage frontend.
 * This is intentionally not a CompilationModel and makes no executable-coverage claim.
 */
export class MidnightSource {
  constructor(compilation, config) {
    this.compilation = compilation
    this.config = config
    this.nodes = new Map()
    this.files = new Map()
    this.parents = new Map()
    for (const [file, source] of sortedEntries(compilation.output.sources ?? {})) {
      assert(Number.isInteger(source.id) && !this.files.has(source.id), 'duplicate/missing source ID', source.ast)
      this.files.set(source.id, file)
      const index = (node, parent) => {
        if (!node || typeof node !== 'object') return
        let next = parent
        if (node.nodeType && Number.isInteger(node.id)) {
          assert(!this.nodes.has(node.id), 'duplicate AST ID', node)
          this.nodes.set(node.id, node)
          if (parent) this.parents.set(node.id, parent.id)
          next = node
        }
        for (const item of Object.values(node)) {
          if (Array.isArray(item)) item.forEach(n => index(n, next))
          else if (item && typeof item === 'object') index(item, next)
        }
      }
      index(source.ast, null)
    }
    const contracts = [...this.nodes.values()].filter(n => n.nodeType === 'ContractDefinition' && n.name === config.contract && this.origin(n).file === config.entrySource)
    assert(contracts.length === 1, 'missing/ambiguous entry contract')
    this.contract = contracts[0]
    this.artifact = compilation.output.contracts?.[config.entrySource]?.[config.contract]
    assert(this.artifact?.storageLayout && this.artifact?.abi && this.artifact?.evm?.methodIdentifiers, 'missing compiler metadata', this.contract)
    this.layoutTypes = this.artifact.storageLayout.types
    this.typeCache = new Map()
    this.layoutCache = new Map()
    this.state = new Map()
    for (const record of this.artifact.storageLayout.storage) {
      const decl = this.declaration(record.astId, 'VariableDeclaration')
      assert(decl.stateVariable && !decl.constant && decl.mutability !== 'immutable', 'invalid storage declaration', decl)
      assert(!this.state.has(decl.id), 'duplicate storage AST reference', decl)
      assert(record.label === decl.name, 'storage label/AST drift', decl)
      const physical = this.layout(record.type)
      this.validateLayoutType(this.type(decl.typeName), physical, decl)
      assert(Number.isInteger(record.offset) && record.offset >= 0 && record.offset < 32, 'invalid storage byte offset', decl)
      assert(/^\d+$/.test(record.slot), 'invalid storage slot', decl)
      assert(physical.kind === 'scalar' ? record.offset + physical.bytes <= 32 : record.offset === 0, 'invalid packed placement', decl)
      this.state.set(decl.id, { declaration: decl.id, slot: record.slot, offset: record.offset, typeId: record.type, physical, origin: this.origin(decl) })
    }
    for (const decl of this.declarations().filter(n => n.nodeType === 'VariableDeclaration' && n.stateVariable && !n.constant && n.mutability !== 'immutable')) {
      assert(this.state.has(decl.id), 'missing storage declaration', decl)
    }
  }
  declaration(id, kind) {
    const node = this.nodes.get(id)
    assert(node && (!kind || node.nodeType === kind), `unresolved declaration ${id}, expected ${kind ?? 'AST node'}`, node)
    return node
  }
  resolve(ref, kind) { return this.declaration(ref?.referencedDeclaration, kind) }
  origin(node, syntheticPolicy = null) {
    assert(Number.isInteger(node.id), 'origin requires a typed source node', node)
    const match = /^(\d+):(\d+):(\d+)$/.exec(node.src ?? '')
    assert(match, 'invalid source span', node)
    const [, startText, sizeText, sourceText] = match
    const file = this.files.get(Number(sourceText))
    const content = this.compilation.input.sources[file]?.content
    assert(typeof content === 'string', 'source bytes unavailable for origin', node)
    const bytes = Buffer.from(content, 'utf8')
    const start = Number(startText), size = Number(sizeText)
    assert(Number.isSafeInteger(start) && Number.isSafeInteger(size) && start + size <= bytes.length, 'span outside pinned source', node)
    return { file, sourceId: Number(sourceText), astNodeId: node.id, span: node.src,
      sourceSha256: sha256(bytes), spanSha256: sha256(bytes.subarray(start, start + size)), syntheticPolicy }
  }
  declarations() {
    return this.contract.linearizedBaseContracts.flatMap(id => this.declaration(id, 'ContractDefinition').nodes)
  }
  type(node, stack = new Set()) {
    assert(node, 'missing type node')
    if (node.nodeType === 'VariableDeclaration') return this.type(node.typeName, stack)
    if (this.typeCache.has(node.id)) return this.typeCache.get(node.id)
    assert(!stack.has(node.id), 'recursive type unsupported', node)
    const next = new Set([...stack, node.id])
    let result
    switch (node.nodeType) {
      case 'ElementaryTypeName': {
        const name = node.name
        const uint = /^(u?int)(\d*)$/.exec(name)
        const bytes = /^bytes(\d+)$/.exec(name)
        if (uint) {
          const bits = Number(uint[2] || 256)
          assert(bits >= 8 && bits <= 256 && bits % 8 === 0, 'invalid integer width', node)
          result = { kind: 'scalar', abi: `${uint[1]}${bits}`, bits, signed: uint[1] === 'int' }
        } else if (bytes) {
          const width = Number(bytes[1]); assert(width >= 1 && width <= 32, 'invalid fixed bytes width', node)
          result = { kind: 'scalar', abi: name, bits: width * 8, signed: false, alignment: 'left' }
        } else if (name === 'address') result = { kind: 'scalar', abi: 'address', bits: 160, signed: false }
        else if (name === 'bool') result = { kind: 'scalar', abi: 'bool', bits: 8, signed: false }
        else if (name === 'bytes' || name === 'string') result = { kind: name, abi: name }
        else fail(`unsupported elementary type ${name}`, node)
        break
      }
      case 'UserDefinedTypeName': {
        const decl = this.resolve(node)
        if (decl.nodeType === 'StructDefinition') result = { kind: 'struct', declaration: decl.id,
          members: decl.members.map(m => ({ declaration: m.id, name: m.name, type: this.type(m, next), origin: this.origin(m) })) }
        else if (decl.nodeType === 'ContractDefinition') result = { kind: 'scalar', abi: 'address', bits: 160, signed: false, declaration: decl.id }
        else if (decl.nodeType === 'UserDefinedValueTypeDefinition') result = { ...this.type(decl.underlyingType, next), declaration: decl.id }
        else fail(`unsupported user-defined type ${decl.nodeType}`, node)
        break
      }
      case 'Mapping': result = { kind: 'mapping', key: this.type(node.keyType, next), value: this.type(node.valueType, next) }; break
      case 'ArrayTypeName': {
        const length = node.length ? this.constant(node.length).toString() : null
        assert(length === null || BigInt(length) > 0n, 'invalid array length', node)
        result = { kind: 'array', element: this.type(node.baseType, next), length }
        break
      }
      default: fail(`unsupported type node ${node.nodeType}`, node)
    }
    this.typeCache.set(node.id, result)
    return result
  }
  abiType(type) {
    if (type.abi) return type.abi
    if (type.kind === 'array') return `${this.abiType(type.element)}[${type.length ?? ''}]`
    if (type.kind === 'struct') return `(${type.members.map(m => this.abiType(m.type)).join(',')})`
    fail(`non-ABI type ${type.kind}`)
  }
  constant(node, stack = new Set()) {
    assert(node, 'missing constant expression')
    assert(!stack.has(node.id), 'cyclic constant', node)
    const next = new Set([...stack, node.id])
    const c = n => this.constant(n, next)
    if (node.nodeType === 'Literal' && node.kind === 'number') {
      let literal
      if (/^0x[0-9a-fA-F]+$/.test(node.value)) literal = BigInt(node.value)
      else {
        const m = /^(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/.exec(node.value)
        assert(m, 'unsupported numeric literal', node)
        const exponent = Number(m[3] || 0) - (m[2]?.length || 0)
        assert(Math.abs(exponent) <= 4096, 'literal exponent outside resource limit', node)
        const digits = BigInt(m[1] + (m[2] || ''))
        if (exponent < 0) assert(digits % (10n ** BigInt(-exponent)) === 0n, 'fractional literal needs rational lowering', node)
        literal = exponent >= 0 ? digits * 10n ** BigInt(exponent) : digits / 10n ** BigInt(-exponent)
      }
      const units = { wei: 1n, gwei: 10n ** 9n, ether: 10n ** 18n, seconds: 1n, minutes: 60n, hours: 3600n, days: 86400n, weeks: 604800n }
      assert(!node.subdenomination || units[node.subdenomination], 'unknown literal denomination', node)
      return literal * (node.subdenomination ? units[node.subdenomination] : 1n)
    }
    if (node.nodeType === 'Identifier' || node.nodeType === 'MemberAccess' && node.referencedDeclaration >= 0) {
      const decl = this.resolve(node, 'VariableDeclaration')
      assert(decl.constant, 'not a constant declaration', node)
      return c(decl.value)
    }
    if (node.nodeType === 'FunctionCall' && node.kind === 'typeConversion') {
      assert(node.arguments.length === 1 && node.expression.nodeType === 'ElementaryTypeNameExpression', 'unsupported constant conversion', node)
      const type = this.type(node.expression.typeName)
      assert(type.kind === 'scalar' && !type.signed && !type.alignment, 'unsupported constant conversion target', node)
      const value = c(node.arguments[0]), modulus = 1n << BigInt(type.bits)
      return ((value % modulus) + modulus) % modulus
    }
    if (node.nodeType === 'FunctionCall' && node.expression.nodeType === 'Identifier' && node.expression.name === 'keccak256' && node.expression.referencedDeclaration < 0) {
      assert(node.arguments.length === 1 && node.arguments[0].nodeType === 'Literal' && ['string', 'unicodeString', 'hexString'].includes(node.arguments[0].kind), 'constant keccak requires literal bytes', node)
      return BigInt(`0x${keccak256(Buffer.from(node.arguments[0].hexValue, 'hex'))}`)
    }
    if (node.nodeType === 'MemberAccess' && ['max', 'min'].includes(node.memberName) && node.expression.nodeType === 'FunctionCall' && node.expression.expression.name === 'type' && node.expression.expression.referencedDeclaration < 0) {
      assert(node.expression.arguments.length === 1 && node.expression.arguments[0].nodeType === 'ElementaryTypeNameExpression', 'unsupported type bound', node)
      const type = this.type(node.expression.arguments[0].typeName)
      assert(type.kind === 'scalar' && /^u?int/.test(type.abi), 'noninteger bound', node)
      return node.memberName === 'max' ? (1n << BigInt(type.bits - (type.signed ? 1 : 0))) - 1n : type.signed ? -(1n << BigInt(type.bits - 1)) : 0n
    }
    if (node.nodeType === 'UnaryOperation') {
      const value = c(node.subExpression)
      if (node.operator === '-') return -value
      if (node.operator === '+') return value
      if (node.operator === '~') return ~value
    }
    if (node.nodeType === 'BinaryOperation') {
      const l = c(node.leftExpression), r = c(node.rightExpression)
      switch (node.operator) {
        case '+': return l + r
        case '-': return l - r
        case '*': return l * r
        case '/':
          assert(r !== 0n, 'division by zero', node)
          assert(!node.typeDescriptions?.typeIdentifier?.startsWith('t_rational') || l % r === 0n, 'fractional constant needs rational lowering', node)
          return l / r
        case '%': assert(r !== 0n, 'zero constant modulus', node); return l % r
        case '**': assert(r >= 0n && r <= 4096n, 'constant exponent outside resource limit', node); return l ** r
        case '<<': assert(r >= 0n && r <= 4096n, 'constant shift outside resource limit', node); return l << r
        case '>>': assert(r >= 0n && r <= 4096n, 'constant shift outside resource limit', node); return l >> r
        case '|': return l | r
        case '&': return l & r
        case '^': return l ^ r
      }
    }
    fail('unsupported constant expression (no guessed value)', node)
  }
  layout(id, stack = new Set()) {
    if (this.layoutCache.has(id)) return this.layoutCache.get(id)
    const raw = this.layoutTypes[id]
    assert(raw, `missing storage type ${id}`)
    assert(!stack.has(id), `recursive storage layout ${id}`)
    assert(/^\d+$/.test(raw.numberOfBytes), `invalid storage size ${id}`)
    const next = new Set([...stack, id]), descend = key => this.layout(key, next)
    const bytes = Number(raw.numberOfBytes)
    assert(Number.isSafeInteger(bytes) && bytes > 0, `invalid layout size ${id}`)
    let result
    if (raw.encoding === 'mapping') {
      assert(bytes === 32, 'mapping anchor must occupy one word')
      result = { kind: 'mapping', key: descend(raw.key), value: descend(raw.value), bytes }
    } else if (raw.encoding === 'inplace' && raw.members) {
      const members = raw.members.map(m => {
        const decl = this.declaration(m.astId, 'VariableDeclaration')
        assert(m.label === decl.name && /^\d+$/.test(m.slot) && Number.isInteger(m.offset) && m.offset >= 0 && m.offset < 32, 'invalid member layout', decl)
        const type = descend(m.type)
        assert(BigInt(m.slot) * 32n + BigInt(m.offset + type.bytes) <= BigInt(bytes), 'member outside struct layout', decl)
        return { declaration: decl.id, slot: m.slot, offset: m.offset, typeId: m.type, type, origin: this.origin(decl) }
      })
      result = { kind: 'struct', members, bytes }
    } else if (raw.encoding === 'inplace' && raw.base) {
      // Array extent is resolved from the AST during validation, never inferred from a label.
      result = { kind: 'fixedArray', element: descend(raw.base), bytes }
    } else if (raw.encoding === 'dynamic_array') result = { kind: 'dynamicArray', element: descend(raw.base), bytes }
    else if (raw.encoding === 'bytes') result = { kind: 'bytes', bytes }
    else if (raw.encoding === 'inplace') {
      assert(bytes <= 32, `unsupported in-place storage type ${id}`)
      result = { kind: 'scalar', label: raw.label, bytes }
    } else fail(`unknown storage encoding ${raw.encoding}`)
    this.layoutCache.set(id, result)
    return result
  }
  validateLayoutType(type, physical, node) {
    const validate = (a, b, n = node) => this.validateLayoutType(a, b, n)
    if (type.kind === 'scalar') {
      assert(physical.kind === 'scalar' && physical.bytes * 8 === type.bits, 'scalar storage width drift', node)
      // Contract types are ABI addresses but have a contract label in solc layout.
      const decl = type.declaration && this.declaration(type.declaration)
      const label = decl?.nodeType === 'ContractDefinition' ? `contract ${decl.name}` : type.abi
      assert(physical.label === label, 'scalar storage type drift', node)
    } else if (type.kind === 'mapping') {
      assert(physical.kind === 'mapping', 'mapping storage kind drift', node)
      validate(type.key, physical.key); validate(type.value, physical.value)
    } else if (type.kind === 'struct') {
      assert(physical.kind === 'struct' && type.members.length === physical.members.length, 'struct layout/member count drift', node)
      type.members.forEach((member, i) => {
        const p = physical.members[i]
        assert(p.declaration === member.declaration, 'struct member reference/order drift', node)
        validate(member.type, p.type, this.declaration(member.declaration))
      })
    } else if (type.kind === 'array') {
      assert(physical.kind === (type.length === null ? 'dynamicArray' : 'fixedArray'), 'array storage kind drift', node)
      validate(type.element, physical.element)
      if (type.length !== null) {
        const count = BigInt(type.length), elem = physical.element
        const expected = elem.kind === 'scalar' ? ((count + BigInt(Math.floor(32 / elem.bytes)) - 1n) / BigInt(Math.floor(32 / elem.bytes))) * 32n
          : count * BigInt(elem.bytes)
        assert(expected === BigInt(physical.bytes), 'fixed-array size/packing drift', node)
      }
    } else assert(physical.kind === 'bytes', 'dynamic bytes storage drift', node)
  }
  /** Exact storage address plan. Runtime indices still require type cleanup/bounds checks.
   * Offsets are byte offsets from the least significant end of an EVM storage word.
   */
  storagePath(declarationId, steps) {
    const anchor = this.state.get(declarationId)
    assert(anchor, `unknown storage declaration ${declarationId}`)
    let address = { op: 'literal', value: anchor.slot }, offset = { op: 'literal', value: String(anchor.offset) }
    let type = this.type(this.declaration(declarationId)), physical = anchor.physical
    const guards = []
    for (const step of steps) {
      if (type.kind === 'mapping') {
        assert(step.kind === 'index', 'mapping requires typed index')
        address = { op: 'mappingHash', key: step.expression, keyType: type.key, slot: address }
        offset = { op: 'literal', value: '0' }; type = type.value; physical = physical.value
      } else if (type.kind === 'struct') {
        assert(step.kind === 'member', 'struct requires declaration reference')
        const i = type.members.findIndex(m => m.declaration === step.declaration)
        assert(i >= 0, `member ${step.declaration} not in struct ${type.declaration}`)
        const member = physical.members[i]
        address = { op: 'add', left: address, right: { op: 'literal', value: member.slot } }
        offset = { op: 'literal', value: String(member.offset) }; type = type.members[i].type; physical = member.type
      } else if (type.kind === 'array' && type.length !== null) {
        assert(step.kind === 'index', 'array requires index')
        guards.push({ op: 'lt', left: step.expression, right: { op: 'literal', value: type.length }, panic: '0x32' })
        const elem = physical.element, packing = elem.kind === 'scalar' ? Math.floor(32 / elem.bytes) : 1
        address = { op: 'add', left: address, right: packing > 1
          ? { op: 'div', left: step.expression, right: { op: 'literal', value: String(packing) } }
          : { op: 'mul', left: step.expression, right: { op: 'literal', value: String(Math.ceil(elem.bytes / 32)) } } }
        offset = packing > 1 ? { op: 'mul', left: { op: 'mod', left: step.expression, right: { op: 'literal', value: String(packing) } }, right: { op: 'literal', value: String(elem.bytes) } } : { op: 'literal', value: '0' }
        type = type.element; physical = elem
      } else fail(`unsupported storage path through ${type.kind}`)
    }
    return { address, offset, type, physical, guards, origin: anchor.origin }
  }
  abiCanonical(param) {
    if (param.type.startsWith('tuple')) return `(${param.components.map(p => this.abiCanonical(p)).join(',')})${param.type.slice(5)}`
    return param.type
  }
  surface() {
    const declarations = this.declarations()
    const functions = declarations.filter(n => n.nodeType === 'FunctionDefinition' && n.body && ['public', 'external'].includes(n.visibility) && n.kind === 'function')
    const getters = declarations.filter(n => n.nodeType === 'VariableDeclaration' && n.stateVariable && n.visibility === 'public')
    const seen = new Set()
    const reachableReferences = new Set()
    for (const fn of this.callGraph()) walk(this.declaration(fn.declaration).body, n => {
      if (Number.isInteger(n.referencedDeclaration) && n.referencedDeclaration >= 0) reachableReferences.add(n.referencedDeclaration)
    })
    return this.artifact.abi.map(abi => {
      let matches, syntheticPolicy = null
      if (abi.type === 'function') {
        const signature = `${abi.name}(${abi.inputs.map(p => this.abiCanonical(p)).join(',')})`
        matches = functions.filter(n => `${n.name}(${n.parameters.parameters.map(p => this.abiType(this.type(p))).join(',')})` === signature)
        if (matches.length === 0) {
          matches = getters.filter(n => n.name === abi.name)
          syntheticPolicy = 'solidity-public-state-getter-v1'
          if (matches.length === 1) this.validateGetter(matches[0], abi)
        }
        assert(matches.length === 1, `ambiguous/unresolved ABI function ${signature}`)
        const decl = matches[0]
        assert(decl.functionSelector === this.artifact.evm.methodIdentifiers[signature] && decl.functionSelector === keccak256(Buffer.from(signature)).slice(0, 8), 'AST/ABI selector drift', decl)
        if (!syntheticPolicy) assert(abi.outputs.map(p => this.abiCanonical(p)).join(',') === decl.returnParameters.parameters.map(p => this.abiType(this.type(p))).join(','), 'ABI return type drift', decl)
        assert(!seen.has(decl.id), 'duplicate public declaration', decl); seen.add(decl.id)
        return { abi, signature, selector: decl.functionSelector, declaration: decl.id, origin: this.origin(decl, syntheticPolicy) }
      }
      if (abi.type === 'constructor') matches = declarations.filter(n => n.nodeType === 'FunctionDefinition' && n.kind === 'constructor')
      else if (abi.type === 'error' || abi.type === 'event') {
        const kind = abi.type === 'error' ? 'ErrorDefinition' : 'EventDefinition'
        matches = [...this.nodes.values()].filter(n => n.nodeType === kind && n.name === abi.name && n.parameters.parameters.map(p => this.abiType(this.type(p))).join(',') === abi.inputs.map(p => this.abiCanonical(p)).join(','))
        if (matches.length > 1) matches = matches.filter(n => reachableReferences.has(n.id))
        if (abi.type === 'event' && matches.length === 1) assert(matches[0].anonymous === abi.anonymous && matches[0].parameters.parameters.every((p, i) => p.indexed === abi.inputs[i].indexed), 'event indexed/order drift', matches[0])
      } else fail(`unsupported ABI entry ${abi.type}`)
      assert(matches.length === 1, `unresolved/ambiguous ABI ${abi.type} ${abi.name ?? ''}`)
      return { abi, declaration: matches[0].id, origin: this.origin(matches[0]) }
    }).concat((() => {
      assert(functions.every(n => seen.has(n.id)) && getters.every(n => seen.has(n.id)), 'incomplete ABI surface')
      return []
    })())
  }
  validateGetter(decl, abi) {
    let type = this.type(decl), args = []
    while (type.kind === 'mapping' || type.kind === 'array') {
      args.push(type.kind === 'mapping' ? this.abiType(type.key) : 'uint256')
      type = type.kind === 'mapping' ? type.value : type.element
    }
    const outputs = type.kind === 'struct' ? type.members.filter(m => !['mapping', 'array'].includes(m.type.kind)).map(m => this.abiType(m.type)) : [this.abiType(type)]
    assert(args.join(',') === abi.inputs.map(p => this.abiCanonical(p)).join(','), 'getter input drift', decl)
    assert(outputs.join(',') === abi.outputs.map(p => this.abiCanonical(p)).join(','), 'getter output drift', decl)
  }
  callGraph() {
    const roots = this.declarations().filter(n => n.nodeType === 'FunctionDefinition' && n.body && (n.kind === 'constructor' || ['external', 'public'].includes(n.visibility)))
    const pending = [...roots], visited = new Map()
    while (pending.length) {
      const decl = pending.shift()
      if (visited.has(decl.id)) continue
      const calls = [], nodeKinds = new Set(), inlineAssembly = []
      walk(decl.body, node => {
        nodeKinds.add(node.nodeType)
        if (node.nodeType === 'InlineAssembly') inlineAssembly.push({ origin: this.origin(node), externalReferences: node.externalReferences, ast: node.AST })
        if (node.nodeType !== 'FunctionCall' || node.kind !== 'functionCall') return
        const target = node.expression.referencedDeclaration
        if (Number.isInteger(target) && target >= 0) {
          const callee = this.declaration(target)
          assert(['FunctionDefinition', 'ErrorDefinition', 'EventDefinition', 'VariableDeclaration'].includes(callee.nodeType), 'unsupported call target declaration', node)
          calls.push({ call: node.id, target, kind: callee.nodeType, origin: this.origin(node) })
          if (callee.nodeType === 'FunctionDefinition' && callee.body) pending.push(callee)
        } else calls.push({ call: node.id, target: null, kind: 'builtin-or-low-level', expressionKind: node.expression.nodeType, member: node.expression.memberName ?? node.expression.name, origin: this.origin(node) })
      })
      visited.set(decl.id, { declaration: decl.id, name: decl.name, origin: this.origin(decl), calls, nodeKinds: [...nodeKinds].sort(), inlineAssembly })
    }
    return [...visited.values()].sort((a, b) => a.declaration - b.declaration)
  }
  manifest() {
    const immutables = this.declarations().filter(n => n.nodeType === 'VariableDeclaration' && n.mutability === 'immutable')
    return {
      schemaVersion: 1, stage: 'validated-source-frontend-not-executable-model',
      sourceCommit: this.compilation.commit, compiler: { version: this.compilation.compiler.version, sha256: this.compilation.compiler.sha256 },
      sourceFiles: Object.fromEntries(sortedEntries(this.compilation.input.sources).map(([file, source]) => [file, sha256(source.content)])),
      contract: this.origin(this.contract), abi: this.surface(), storage: [...this.state.values()],
      immutables: immutables.map(n => ({ declaration: n.id, type: this.type(n), origin: this.origin(n), loweringPolicy: 'unassigned-requires-explicit-constructor-only-synthetic-storage-policy' })),
      reachableFunctions: this.callGraph(),
    }
  }
}
