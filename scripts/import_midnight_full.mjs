#!/usr/bin/env node
/** Complete pinned-source AST emitter. Generation is not a claim of semantic
 * equivalence or a passing compiler/parity run. No handwritten model is read.
 * --output PATH emits a standalone Lean file for probes. No arguments writes
 * the configured Lean/ABI/provenance bundle; --check compares without writing.
 */
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs'
import { resolve, dirname } from 'node:path'
import { pathToFileURL } from 'node:url'
import { createHash } from 'node:crypto'
import { compilePinnedSource, MidnightSource } from './lib/midnight-source.mjs'
import { MidnightLowering } from './lib/midnight-lowering.mjs'
import * as abi from './lib/midnight-abi.mjs'
import { lowerCall, lowerMember, lowerAssembly, supportDefinitions } from './lib/midnight-builtins.mjs'

const hash = text => createHash('sha256').update(text).digest('hex')
const ensure = (condition, message) => { if (!condition) throw new Error(message) }
const json = value => JSON.stringify(value, (_, v) => typeof v === 'bigint' ? v.toString() : v, 2) + '\n'
const generatorFiles = [
  'config/midnight-full-import.json', 'scripts/import_midnight_full.mjs',
  'scripts/lib/keccak256.mjs', 'scripts/lib/midnight-abi.mjs',
  'scripts/lib/midnight-builtins.mjs', 'scripts/lib/midnight-lowering.mjs', 'scripts/lib/midnight-cei.mjs',
  'scripts/lib/midnight-source.mjs',
]

export function fullLowering(source) {
  return new MidnightLowering(source, {
    abi, call: lowerCall, member: lowerMember, statement: lowerAssembly, supportDefinitions,
    // Transparent statement definitions keep the pinned Lean elaborator's work
    // bounded per term; exact expansion equivalence is checked by the native gate.
    factorStatements: true,
    abiRequire(condition, ctx) {
      ctx.emit(`.ite (.logicalNot ${condition}) [.unsafeYul (UnsafeYulFragment.rawRevert (.lit 0) (.lit 0) { name := ${JSON.stringify(ctx.fresh('abi_decode_failure'))}, obligation := "Malformed ABI input reverts with the empty memory slice; this boundary terminates and is not an external interaction.", proofStatus := .assumed })] []`)
    },
  })
}

/** Record typed declaration identities at the emission boundary, rather than
 * guessing identities/selectors by parsing rendered function names or bodies. */
function captureDeclarations(lowering, source) {
  const functions = [], constructors = [], statements = []
  const captureStatements = (start, decl) => {
    for (const [offset, text] of (lowering.statementDefinitions?.slice(start) ?? []).entries()) {
      const name = `sourceStatement_${start + offset}`
      ensure(text.startsWith(`def ${name} : Stmt := `), 'statement factoring/provenance identity drift')
      statements.push({ generatedDeclaration: name, kind: 'transparent-CM-statement',
        origin: source.origin(decl, 'transparent-CM-statement-factoring-v1'),
        compilationModelSha256: hash(text) })
    }
  }
  const compile = lowering.compileFunction.bind(lowering)
  const getter = lowering.compileGetter.bind(lowering)
  lowering.compileFunction = (decl, options = {}) => {
    const start = lowering.statementDefinitions?.length ?? 0
    const code = compile(decl, options)
    captureStatements(start, decl)
    const internal = Boolean(options.internal || !['public', 'external'].includes(decl.visibility))
    const entry = {
      declaration: decl.id, compilationModelName: lowering.functionName(decl, Boolean(options.internal)),
      internal, origin: source.origin(decl),
      policy: internal ? 'scalar-or-ABI-memory-pointer-v1' : 'source-ABI-calldata-v1',
      compilationModelSha256: hash(code),
      ceiPolicy: lowering.ceiPolicies.get(decl.id),
      mutability: lowering.functionMutability.get(lowering.functionName(decl, Boolean(options.internal))),
    }
    if (decl.kind === 'constructor') constructors.push(entry)
    else functions.push(entry)
    return code
  }
  lowering.compileGetter = decl => {
    const start = lowering.statementDefinitions?.length ?? 0
    const code = getter(decl)
    captureStatements(start, decl)
    functions.push({ declaration: decl.id, compilationModelName: decl.name, internal: false,
      origin: source.origin(decl, 'solidity-public-state-getter-v1'),
      policy: 'solidity-public-state-getter-v1', compilationModelSha256: hash(code),
      mutability: { sourceMutability: 'view', modelMutability: 'view', policy: 'source-mutability-preserved-v1', reasons: [] } })
    return code
  }
  return { functions, constructors, statements }
}

export function generateFull(root = process.cwd()) {
  const config = JSON.parse(readFileSync(resolve(root, 'config/midnight-full-import.json'), 'utf8'))
  const source = new MidnightSource(compilePinnedSource(root, config), config)
  const lowering = fullLowering(source), captured = captureDeclarations(lowering, source)
  const result = lowering.generate(), surface = source.surface(), graph = source.callGraph()
  ensure(captured.functions.length === result.functions.length, 'function provenance/emission count drift')
  ensure(captured.constructors.length === 1 && result.constructor, 'constructor provenance/emission drift')
  const names = new Set()
  captured.functions.forEach((entry, i) => {
    ensure(!names.has(entry.compilationModelName), `ambiguous generated function ${entry.compilationModelName}`)
    names.add(entry.compilationModelName)
    entry.generatedDeclaration = `sourceFunction_${i}`
  })
  const emitted = new Set([...captured.functions, ...captured.constructors].map(f => f.declaration))
  ensure(graph.every(f => emitted.has(f.declaration)), 'reachable source function omitted')
  const publicEntries = captured.functions.filter(f => !f.internal)
  const publicSurface = surface.filter(row => row.abi.type === 'function')
  const byId = new Map(publicSurface.map(row => [row.declaration, row]))
  ensure(publicEntries.length === publicSurface.length && new Set(publicEntries.map(f => f.declaration)).size === byId.size,
    'public ABI coverage drift')
  const selectors = publicEntries.map(f => {
    const row = byId.get(f.declaration)
    ensure(row, `generated public declaration ${f.declaration} missing from compiler ABI`)
    return BigInt('0x' + row.selector).toString()
  })
  result.lean = result.lean.replace('end Midnight.Generated.Full',
    `def selectors : List Nat := [${selectors.join(', ')}]\n` +
    `def sourceAbiJSON : String := ${JSON.stringify(json(source.artifact.abi))}\n` +
    'end Midnight.Generated.Full')
  result.functionEntries = captured.functions
  result.selectors = selectors

  const base = source.manifest()
  const generatorInputs = Object.fromEntries(generatorFiles.map(file => [file, hash(readFileSync(resolve(root, file)))]))
  const declarations = [...captured.functions.map(f => ({ ...f, kind: 'function' })), ...captured.statements]
  const helperNames = [...result.lean.matchAll(/^def ([A-Za-z_][A-Za-z0-9_]*)\b/gm)].map(m => m[1])
  ensure(new Set(helperNames).size === helperNames.length, 'duplicate generated Lean declaration')
  const functionNames = new Set(declarations.map(d => d.generatedDeclaration))
  for (const name of helperNames) {
    if (functionNames.has(name)) continue
    ensure(['spec', 'selectors', 'sourceAbiJSON'].includes(name) || /^midnightOpcode_[a-z0-9]+$/.test(name),
      `generated declaration lacks a provenance policy: ${name}`)
    declarations.push({ generatedDeclaration: name,
      kind: name.startsWith('midnightOpcode_') ? 'explicit-EVM-scaffolding' : 'model-or-ABI-metadata',
      origin: source.origin(source.contract, name.startsWith('midnightOpcode_') ? 'source-opcode-and-ABI-scaffolding-v1' : 'typed-source-model-assembly-v1'),
      sourceUses: name.startsWith('midnightOpcode_') ? graph.flatMap(f => [...f.inlineAssembly.map(a => a.origin), ...f.calls.filter(c => c.target === null).map(c => c.origin)]) : undefined,
    })
  }
  ensure(functionNames.size === captured.functions.length + captured.statements.length && [...functionNames].every(n => helperNames.includes(n)),
    'function definition/provenance identity drift')
  ensure(declarations.length === helperNames.length, 'generated declaration provenance is incomplete')
  const fields = [...lowering.storage].map(([id, field], i) => ({
    declaration: id, generatedName: field.name,
    origin: source.origin(source.declaration(id), field.immutable ? 'immutable-to-reserved-storage-1024-v1' : null),
    sourceLayout: source.state.get(id) ?? null,
    compilationModelSha256: hash(result.fields[i]),
    leaves: field.leaves.map(leaf => ({ ...leaf, origin: source.origin(source.declaration(
      [...leaf.steps].reverse().find(s => s.kind === 'member')?.declaration ?? id)) })),
  }))
  ensure(fields.length === result.fields.length, 'storage provenance/emission count drift')
  const manifest = {
    ...base, schemaVersion: 2, stage: 'complete-source-derived-compilation-model',
    verificationClaim: 'Generation only; compiler, parity and semantic review are separate gates. No Solidity-equivalence theorem is claimed.',
    generatorInputs,
    compilerInputSha256: hash(json(source.compilation.input)),
    compilerAstSha256: hash(json(source.compilation.output.sources)),
    storageLayoutSha256: hash(json(source.artifact.storageLayout)),
    generatedLeanSha256: hash(result.lean), generatedAbiSha256: hash(json(source.artifact.abi)),
    policies: result.policies,
    declarations, constructor: captured.constructors[0], fields,
    eventsAndErrors: surface.filter(row => ['event', 'error'].includes(row.abi.type)),
    syntheticErrors: [{ name: 'Panic', origin: source.origin(source.contract, 'solidity-runtime-panic-checks-v1') }],
    immutables: base.immutables.map(entry => ({ ...entry, loweringPolicy: 'immutable-to-reserved-storage-1024-v1' })),
    publicSelectorsInCompilationOrder: publicEntries.map((entry, i) => ({ declaration: entry.declaration,
      signature: byId.get(entry.declaration).signature, selector: byId.get(entry.declaration).selector,
      compilationModelName: entry.compilationModelName, value: selectors[i] })),
  }
  return { source, result, config, manifest: JSON.parse(json(manifest)) }
}

function main(args) {
  let check = false, output = null
  while (args.length) {
    const arg = args.shift()
    if (arg === '--check' && !check) check = true
    else if (arg === '--output' && output === null && args.length) output = args.shift()
    else throw new Error('Usage: node scripts/import_midnight_full.mjs [--check] [--output PATH]')
  }
  const root = process.cwd(), { source, result, config, manifest } = generateFull(root)
  const artifacts = output ? [[resolve(root, output), result.lean]] : [
    [resolve(root, config.leanOutput), result.lean],
    [resolve(root, config.abiOutput), json(source.artifact.abi)],
    [resolve(root, config.manifestOutput), json(manifest)],
  ]
  // All source analysis and rendering completes before any destination is touched.
  for (const [path, content] of artifacts) {
    if (check) ensure(readFileSync(path, 'utf8') === content, `generated artifact drift: ${path}`)
    else { mkdirSync(dirname(path), { recursive: true }); writeFileSync(path, content) }
  }
  console.log(json({ outputs: artifacts.map(([path]) => path), checked: check,
    reachableSourceFunctions: source.callGraph().length, generatedFunctionEntries: result.functions.length,
    storageFields: result.fields.length, generatedDeclarationsWithProvenance: manifest.declarations.length,
    status: 'generated; compiler/parity verification is separate' }).trim())
}
if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  try { main(process.argv.slice(2)) } catch (error) { console.error(error.message); process.exitCode = 1 }
}
