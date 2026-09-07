#!/usr/bin/env node

import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import {
  chmodSync,
  copyFileSync,
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  writeFileSync,
} from 'node:fs'
import { dirname, relative, resolve } from 'node:path'

const ROOT = resolve(import.meta.dirname, '..')
const CONFIG_PATH = resolve(ROOT, 'config/midnight-admin-import.json')
const config = JSON.parse(readFileSync(CONFIG_PATH, 'utf8'))
const sourceRoot = resolve(ROOT, config.sourceRoot)
const entrySource = config.entrySource
const leanOutput = resolve(ROOT, config.leanOutput)
const manifestOutput = resolve(ROOT, config.manifestOutput)
const checkOnly = process.argv.includes('--check')

const sha256 = value => createHash('sha256').update(value).digest('hex')
const quote = value => JSON.stringify(value)
const compareText = (left, right) => (left < right ? -1 : left > right ? 1 : 0)

function die(message, code = 2) {
  process.stderr.write(`ERROR: ${message}\n`)
  process.exit(code)
}

function unsupported(node, fileBySourceId, reason) {
  const sourceId = Number(String(node?.src ?? '0:0:-1').split(':')[2])
  const file = fileBySourceId.get(sourceId) ?? '<unknown>'
  process.stderr.write(
    `UNSUPPORTED FEATURE: ${reason}; nodeType=${node?.nodeType ?? '<unknown>'}; ` +
      `id=${node?.id ?? '<unknown>'}; source=${file}; span=${node?.src ?? '<unknown>'}\n`,
  )
  process.exit(3)
}

function stableJson(value) {
  const canonical = current => {
    if (Array.isArray(current)) return current.map(canonical)
    if (current && typeof current === 'object') {
      return Object.fromEntries(
        Object.keys(current)
          .sort()
          .map(key => [key, canonical(current[key])]),
      )
    }
    return current
  }
  return `${JSON.stringify(canonical(value), null, 2)}\n`
}

function collectSoliditySources(directory) {
  const sources = {}
  const walk = current => {
    for (const entry of readdirSync(current, { withFileTypes: true }).sort((a, b) => compareText(a.name, b.name))) {
      const path = resolve(current, entry.name)
      if (entry.isDirectory()) walk(path)
      else if (entry.isFile() && entry.name.endsWith('.sol')) {
        const key = relative(sourceRoot, path).replaceAll('\\', '/')
        sources[key] = { content: readFileSync(path, 'utf8') }
      }
    }
  }
  walk(resolve(sourceRoot, 'src'))
  return sources
}

function commandOutput(command, args, options = {}) {
  const result = spawnSync(command, args, {
    encoding: 'utf8',
    maxBuffer: 80_000_000,
    ...options,
  })
  if (result.status !== 0) {
    die(`${command} ${args.join(' ')} failed: ${result.stderr || result.stdout}`)
  }
  return result.stdout.trim()
}

function solcMatches(path) {
  if (!existsSync(path)) return false
  const version = spawnSync(path, ['--version'], { encoding: 'utf8' })
  if (version.status !== 0 || !version.stdout.includes(config.solc.version)) return false
  return sha256(readFileSync(path)) === config.solc.sha256
}

async function resolveSolc() {
  const cachePath = resolve(ROOT, `.cache/solc-${config.solc.version}`)
  const candidates = [
    process.env.MORPHO_SOLC_0_8_34,
    cachePath,
    'solc',
  ].filter(Boolean)

  for (const candidate of candidates) {
    if (candidate === 'solc') {
      const lookup = spawnSync('sh', ['-c', 'command -v solc'], { encoding: 'utf8' })
      if (lookup.status === 0 && solcMatches(lookup.stdout.trim())) {
        mkdirSync(dirname(cachePath), { recursive: true })
        copyFileSync(lookup.stdout.trim(), cachePath)
        chmodSync(cachePath, 0o755)
        return cachePath
      }
    } else if (solcMatches(candidate)) {
      if (candidate !== cachePath) {
        mkdirSync(dirname(cachePath), { recursive: true })
        copyFileSync(candidate, cachePath)
        chmodSync(cachePath, 0o755)
      }
      return cachePath
    }
  }

  process.stderr.write(`Downloading pinned solc ${config.solc.version}...\n`)
  const response = await fetch(config.solc.linuxAmd64Url)
  if (!response.ok) die(`could not download pinned solc: HTTP ${response.status}`)
  const bytes = Buffer.from(await response.arrayBuffer())
  if (sha256(bytes) !== config.solc.sha256) die('downloaded solc checksum mismatch')
  mkdirSync(dirname(cachePath), { recursive: true })
  writeFileSync(cachePath, bytes)
  chmodSync(cachePath, 0o755)
  if (!solcMatches(cachePath)) die('downloaded solc version/commit check failed')
  return cachePath
}

function walk(node, visit) {
  if (!node || typeof node !== 'object') return
  if (node.nodeType) visit(node)
  for (const value of Object.values(node)) {
    if (Array.isArray(value)) value.forEach(item => walk(item, visit))
    else if (value && typeof value === 'object') walk(value, visit)
  }
}

function canonicalParamType(param, fileBySourceId) {
  const type = param?.typeDescriptions?.typeString
  if (type === 'address') return 'address'
  unsupported(param, fileBySourceId, `parameter type ${type ?? '<missing>'}`)
}

function sourceOrigin(node, sourceFile) {
  return {
    sourceFile,
    astNodeId: node.id,
    sourceSpan: node.src,
  }
}

function resolveDeclaration(node, nodesById, expectedType, fileBySourceId, context) {
  const id = node?.referencedDeclaration
  const declaration = nodesById.get(id)
  if (!declaration || declaration.nodeType !== expectedType) {
    unsupported(node, fileBySourceId, `${context} must resolve to ${expectedType}`)
  }
  return declaration
}

function fieldNameFor(declaration) {
  return `${declaration.name}Slot`
}

function lowerExpression(node, state) {
  const { nodesById, fileBySourceId, parameters } = state
  if (node?.nodeType === 'MemberAccess' && node.memberName === 'sender') {
    if (node.expression?.nodeType !== 'Identifier' || node.expression.name !== 'msg') {
      unsupported(node, fileBySourceId, 'only msg.sender member access is supported')
    }
    return '.caller'
  }
  if (node?.nodeType === 'Identifier') {
    const declaration = nodesById.get(node.referencedDeclaration)
    if (declaration?.nodeType === 'VariableDeclaration' && declaration.stateVariable === true) {
      if (declaration.typeDescriptions?.typeString !== 'address') {
        unsupported(node, fileBySourceId, 'only address storage reads are supported')
      }
      state.fields.set(declaration.id, declaration)
      return `(.storageAddr ${quote(fieldNameFor(declaration))})`
    }
    if (parameters.has(declaration?.id)) return `(.param ${quote(declaration.name)})`
    unsupported(node, fileBySourceId, 'identifier is neither a selected parameter nor address storage')
  }
  if (node?.nodeType === 'BinaryOperation' && node.operator === '==') {
    return `(.eq ${lowerExpression(node.leftExpression, state)} ${lowerExpression(node.rightExpression, state)})`
  }
  unsupported(node, fileBySourceId, `expression ${node?.nodeType ?? '<missing>'}`)
}

function lowerRequire(statement, state) {
  const call = statement.expression
  if (
    call?.nodeType !== 'FunctionCall' ||
    call.expression?.nodeType !== 'Identifier' ||
    call.expression.name !== 'require' ||
    call.arguments?.length !== 2
  ) {
    unsupported(statement, state.fileBySourceId, 'expected require(condition, CustomError())')
  }
  const errorCall = call.arguments[1]
  if (errorCall?.nodeType !== 'FunctionCall' || errorCall.arguments?.length !== 0) {
    unsupported(errorCall ?? call, state.fileBySourceId, 'custom error arguments are unsupported in this slice')
  }
  const error = resolveDeclaration(
    errorCall.expression,
    state.nodesById,
    'ErrorDefinition',
    state.fileBySourceId,
    'require error',
  )
  if ((error.parameters?.parameters ?? []).length !== 0) {
    unsupported(error, state.fileBySourceId, 'custom error parameters are unsupported in this slice')
  }
  state.errors.set(error.id, error)
  return `.requireError ${lowerExpression(call.arguments[0], state)} ${quote(error.name)} []`
}

function lowerAssignment(statement, state) {
  const assignment = statement.expression
  if (assignment?.nodeType !== 'Assignment' || assignment.operator !== '=') {
    unsupported(statement, state.fileBySourceId, 'expected simple storage assignment')
  }
  const field = resolveDeclaration(
    assignment.leftHandSide,
    state.nodesById,
    'VariableDeclaration',
    state.fileBySourceId,
    'assignment target',
  )
  if (field.stateVariable !== true || field.typeDescriptions?.typeString !== 'address') {
    unsupported(assignment.leftHandSide, state.fileBySourceId, 'assignment target must be address storage')
  }
  state.fields.set(field.id, field)
  return `.setStorageAddr ${quote(fieldNameFor(field))} ${lowerExpression(assignment.rightHandSide, state)}`
}

function lowerEmit(statement, state) {
  const call = statement.eventCall
  if (call?.nodeType !== 'FunctionCall' || call.arguments?.length !== 1) {
    unsupported(statement, state.fileBySourceId, 'expected one-argument event emission')
  }
  const event = resolveDeclaration(
    call.expression,
    state.nodesById,
    'EventDefinition',
    state.fileBySourceId,
    'event call',
  )
  const params = event.parameters?.parameters ?? []
  if (
    params.length !== 1 ||
    params[0].typeDescriptions?.typeString !== 'address' ||
    params[0].indexed !== true
  ) {
    unsupported(event, state.fileBySourceId, 'event must contain one indexed address')
  }
  state.events.set(event.id, event)
  return `.emit ${quote(event.name)} [${lowerExpression(call.arguments[0], state)}]`
}

function lowerFunction(fn, sourceFile, globalState) {
  if (fn.kind !== 'function' || fn.visibility !== 'external' || fn.stateMutability !== 'nonpayable') {
    unsupported(fn, globalState.fileBySourceId, 'selected function must be external and nonpayable')
  }
  if ((fn.modifiers?.length ?? 0) !== 0) {
    unsupported(fn.modifiers[0], globalState.fileBySourceId, 'function modifiers are unsupported in this slice')
  }
  if ((fn.returnParameters?.parameters?.length ?? 0) !== 0) {
    unsupported(fn.returnParameters, globalState.fileBySourceId, 'function return values are unsupported in this slice')
  }
  if (fn.virtual === true) {
    unsupported(fn, globalState.fileBySourceId, 'virtual functions are unsupported in this slice')
  }
  if (!fn.body || fn.body.nodeType !== 'Block') {
    unsupported(fn, globalState.fileBySourceId, 'selected function must have a block body')
  }
  const params = fn.parameters?.parameters ?? []
  const parameters = new Map(params.map(param => [param.id, param]))
  const state = { ...globalState, parameters }
  const body = []
  for (const statement of fn.body?.statements ?? []) {
    if (statement.nodeType === 'ExpressionStatement') {
      if (statement.expression?.nodeType === 'FunctionCall') body.push(lowerRequire(statement, state))
      else if (statement.expression?.nodeType === 'Assignment') body.push(lowerAssignment(statement, state))
      else unsupported(statement, state.fileBySourceId, 'expression statement')
    } else if (statement.nodeType === 'EmitStatement') {
      body.push(lowerEmit(statement, state))
    } else {
      unsupported(statement, state.fileBySourceId, 'function statement')
    }
  }
  if (body.length !== 3 || !body[0].startsWith('.requireError') || !body[1].startsWith('.setStorageAddr') || !body[2].startsWith('.emit')) {
    unsupported(fn.body, state.fileBySourceId, 'expected guard, address write, and event emission in source order')
  }
  body.push('.stop')
  return {
    node: fn,
    sourceFile,
    signature: `${fn.name}(${params.map(param => canonicalParamType(param, state.fileBySourceId)).join(',')})`,
    lean: `{ name := ${quote(fn.name)}\n      params := [${params.map(param => `{ name := ${quote(param.name)}, ty := .address }`).join(', ')}]\n      returnType := none\n      body := [\n        ${body.join(',\n        ')}\n      ] }`,
  }
}

function leanOrigin(origin, kind, name) {
  return `{ kind := ${quote(kind)}, name := ${quote(name)}, sourceFile := ${quote(origin.sourceFile)}, astNodeId := ${origin.astNodeId}, sourceSpan := ${quote(origin.sourceSpan)} }`
}

function sourceFileForNode(node, fileBySourceId) {
  const sourceId = Number(String(node.src).split(':')[2])
  return fileBySourceId.get(sourceId) ?? '<unknown>'
}

function renderLean(lowered, fields, errors, events, origins) {
  const fieldDefs = fields
    .map(({ node, layout }) => `{ name := ${quote(fieldNameFor(node))}, ty := .address, slot := some ${layout.slot} }`)
    .join(',\n    ')
  const errorDefs = errors.map(node => `{ name := ${quote(node.name)}, params := [] }`).join(',\n    ')
  const eventDefs = events
    .map(node => {
      const param = node.parameters.parameters[0]
      return `{ name := ${quote(node.name)}, params := [{ name := ${quote(param.name)}, ty := .address, kind := .indexed }] }`
    })
    .join(',\n    ')
  const functionDefs = lowered.map(item => item.lean).join(',\n    ')
  const originDefs = origins.map(item => leanOrigin(item.origin, item.kind, item.name)).join(',\n    ')

  return `-- GENERATED FILE. DO NOT EDIT.\n-- Run: node scripts/import_midnight_admin_slice.mjs\nimport Compiler.CompilationModel\n\nnamespace Midnight.Generated.AdminSlice\n\nopen Compiler.CompilationModel\n\nstructure DeclarationOrigin where\n  kind : String\n  name : String\n  sourceFile : String\n  astNodeId : Nat\n  sourceSpan : String\n  deriving Repr, BEq\n\ndef sourceCommit : String := ${quote(config.midnightCommit)}\ndef compilerVersion : String := ${quote(config.solc.version)}\n\ndef fields : List Field := [\n    ${fieldDefs}\n  ]\n\ndef errors : List ErrorDef := [\n    ${errorDefs}\n  ]\n\ndef events : List EventDef := [\n    ${eventDefs}\n  ]\n\ndef functions : List FunctionSpec := [\n    ${functionDefs}\n  ]\n\ndef origins : List DeclarationOrigin := [\n    ${originDefs}\n  ]\n\nend Midnight.Generated.AdminSlice\n`
}

async function main() {
  const commit = commandOutput('git', ['-C', sourceRoot, 'rev-parse', 'HEAD'])
  if (commit !== config.midnightCommit) {
    die(`Midnight source pin mismatch: expected ${config.midnightCommit}, got ${commit}`)
  }
  const dirtySources = commandOutput('git', ['-C', sourceRoot, 'status', '--porcelain', '--untracked-files=all', '--', 'src'])
  if (dirtySources !== '') {
    die(`Midnight source tree differs from pinned commit:\n${dirtySources}`)
  }

  const solc = await resolveSolc()
  const sources = collectSoliditySources(sourceRoot)
  const standardInput = {
    language: 'Solidity',
    sources,
    settings: {
      optimizer: { enabled: true, runs: 800 },
      viaIR: true,
      evmVersion: 'osaka',
      metadata: { bytecodeHash: 'none' },
      outputSelection: { '*': { '': ['ast'], '*': ['storageLayout'] } },
    },
  }
  const result = spawnSync(solc, ['--standard-json', '--base-path', sourceRoot, '--allow-paths', sourceRoot], {
    input: JSON.stringify(standardInput),
    encoding: 'utf8',
    maxBuffer: 80_000_000,
  })
  if (result.status !== 0) die(`solc failed: ${result.stderr}`)
  const output = JSON.parse(result.stdout)
  const compilerErrors = (output.errors ?? []).filter(error => error.severity === 'error')
  if (compilerErrors.length > 0) die(compilerErrors.map(error => error.formattedMessage).join('\n'))

  const nodesById = new Map()
  const fileBySourceId = new Map()
  for (const [file, compiledSource] of Object.entries(output.sources ?? {}).sort(([a], [b]) => compareText(a, b))) {
    fileBySourceId.set(compiledSource.id, file)
    walk(compiledSource.ast, node => {
      if (!Number.isInteger(node.id)) return
      if (nodesById.has(node.id)) die(`duplicate AST node id ${node.id}`)
      nodesById.set(node.id, node)
    })
  }

  const entryAst = output.sources?.[entrySource]?.ast
  if (!entryAst) die(`typed AST missing for ${entrySource}`)
  const contract = [...nodesById.values()].find(
    node => node.nodeType === 'ContractDefinition' && node.name === config.contract && sourceFileForNode(node, fileBySourceId) === entrySource,
  )
  if (!contract) die(`contract ${config.contract} not found in ${entrySource}`)

  const functionNodes = (contract.nodes ?? []).filter(node => node.nodeType === 'FunctionDefinition')
  const fields = new Map()
  const errors = new Map()
  const events = new Map()
  const globalState = { nodesById, fileBySourceId, fields, errors, events }
  const lowered = []
  for (const selected of config.functions) {
    const signature = selected.signature
    const matches = functionNodes.filter(node => {
      const params = node.parameters?.parameters ?? []
      const actual = `${node.name}(${params.map(param => param.typeDescriptions?.typeString).join(',')})`
      return actual === signature
    })
    if (matches.length !== 1) die(`${signature}: expected one declaration, found ${matches.length}`)
    if (matches[0].id !== selected.declarationId) {
      die(`${signature}: expected declaration ID ${selected.declarationId}, found ${matches[0].id}`)
    }
    lowered.push(lowerFunction(matches[0], entrySource, globalState))
  }

  const storageLayout = output.contracts?.[entrySource]?.[config.contract]?.storageLayout
  if (!storageLayout) die(`storage layout missing for ${config.contract}`)
  const layoutsByAstId = new Map(storageLayout.storage.map(item => [item.astId, item]))
  const loweredFields = [...fields.values()]
    .map(node => {
      const layout = layoutsByAstId.get(node.id)
      if (!layout) die(`storage layout missing for ${node.name} AST id ${node.id}`)
      const type = storageLayout.types?.[layout.type]
      if (type?.label !== 'address' || layout.offset !== 0) {
        unsupported(node, fileBySourceId, `storage layout must be an unpacked address, got ${type?.label ?? layout.type} at offset ${layout.offset}`)
      }
      return { node, layout: { slot: Number(layout.slot), offset: layout.offset, type: type.label } }
    })
    .sort((a, b) => a.layout.slot - b.layout.slot || a.node.id - b.node.id)
  const loweredErrors = [...errors.values()].sort((a, b) => a.id - b.id)
  const eventOrder = new Map(lowered.map((item, index) => [item.node.name, index]))
  const loweredEvents = [...events.values()].sort((a, b) => {
    const ai = eventOrder.get(`set${a.name.slice(3)}`) ?? Number.MAX_SAFE_INTEGER
    const bi = eventOrder.get(`set${b.name.slice(3)}`) ?? Number.MAX_SAFE_INTEGER
    return ai - bi || a.id - b.id
  })

  const origins = []
  for (const item of loweredFields) origins.push({ kind: 'storage', name: item.node.name, origin: sourceOrigin(item.node, sourceFileForNode(item.node, fileBySourceId)) })
  for (const item of loweredErrors) origins.push({ kind: 'error', name: item.name, origin: sourceOrigin(item, sourceFileForNode(item, fileBySourceId)) })
  for (const item of loweredEvents) origins.push({ kind: 'event', name: item.name, origin: sourceOrigin(item, sourceFileForNode(item, fileBySourceId)) })
  for (const item of lowered) origins.push({ kind: 'function', name: item.signature, origin: sourceOrigin(item.node, item.sourceFile) })

  const lean = renderLean(lowered, loweredFields, loweredErrors, loweredEvents, origins)
  const sourceHashes = Object.fromEntries(
    Object.entries(sources)
      .sort(([a], [b]) => compareText(a, b))
      .map(([file, source]) => [file, sha256(source.content)]),
  )
  const manifest = stableJson({
    schemaVersion: config.schemaVersion,
    compiler: { version: config.solc.version, sha256: config.solc.sha256 },
    source: { root: config.sourceRoot, commit, entry: entrySource, files: sourceHashes },
    settings: standardInput.settings,
    selectedFunctions: lowered.map(item => ({ signature: item.signature, ...sourceOrigin(item.node, item.sourceFile) })),
    declarations: origins.map(item => ({ kind: item.kind, name: item.name, ...item.origin })),
    unsupportedFeatures: [],
    output: { lean: config.leanOutput, sha256: sha256(lean) },
  })

  if (checkOnly) {
    if (!existsSync(leanOutput) || readFileSync(leanOutput, 'utf8') !== lean) die(`generated Lean is stale: ${config.leanOutput}`)
    if (!existsSync(manifestOutput) || readFileSync(manifestOutput, 'utf8') !== manifest) die(`generated manifest is stale: ${config.manifestOutput}`)
  } else {
    mkdirSync(dirname(leanOutput), { recursive: true })
    writeFileSync(leanOutput, lean)
    writeFileSync(manifestOutput, manifest)
  }

  process.stdout.write(`Imported ${lowered.length} Midnight admin functions with 0 unsupported features.\n`)
}

main().catch(error => die(error.stack ?? String(error)))
