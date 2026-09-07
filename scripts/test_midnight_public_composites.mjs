#!/usr/bin/env node
// Independent typed Solidity -> core CM -> actual Lean/Yul -> pinned-solc gate.
// These are ABI prologue/decoder probes, not full Midnight execution or proofs.
import assert from 'node:assert/strict'
import { readFileSync, writeFileSync, mkdtempSync, rmSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { resolve, join } from 'node:path'
import { tmpdir } from 'node:os'
import { createHash } from 'node:crypto'
import { MidnightSource, compilePinnedSource } from './lib/midnight-source.mjs'
import { MidnightLowering } from './lib/midnight-lowering.mjs'
import * as abi from './lib/midnight-abi.mjs'
const root = resolve(import.meta.dirname, '..')
const config = JSON.parse(readFileSync(join(root, 'config/midnight-full-import.json'), 'utf8'))
const solc = process.env.MORPHO_SOLC_0_8_34 || join(root, `.cache/solc-${config.solc.version}`)
assert.equal(createHash('sha256').update(readFileSync(solc)).digest('hex'), config.solc.sha256)
function run(command, args, input, timeout = 30000) {
  const r = spawnSync(command, args, { cwd: root, input, encoding: 'utf8', timeout, maxBuffer: 30000000 })
  assert.equal(r.status, 0, `${r.error ?? ''}\n${r.stdout}\n${r.stderr}`)
  assert.doesNotMatch(r.stdout + r.stderr, /PANIC/)
  return r.stdout
}
function compile(input) {
  const out = JSON.parse(run(solc, ['--standard-json'], JSON.stringify(input)))
  assert.deepEqual((out.errors ?? []).filter(e => e.severity === 'error'), [])
  return out
}
const input = { language: 'Solidity', sources: { 'Probe.sol': { content: `pragma solidity ^0.8.34;
contract Probe {
 struct Dynamic { uint256 tag; bytes data; }
 struct Static { uint256 a; uint256 b; }
 function nested(bytes[] calldata x) external view returns(uint256) { return x[0].length; }
 function raw(bytes calldata x) external view returns(uint256) { return x.length; }
 function dynamicTuple(Dynamic calldata x) external view returns(uint256) { return x.data.length; }
 function fixedWords(uint256 prefix, uint256[2] calldata x) external view returns(uint256) { return x[1]; }
 function staticTuple(uint256 prefix, uint256[2] calldata x, Static calldata y) external view returns(uint256) { return y.b; }
 function fixedDynamic(bytes[2] calldata x) external view returns(uint256) { return x[1].length; }
 function materialize(bytes calldata x) external view returns(uint256) { return helper(x); }
 function helper(bytes memory x) internal pure returns(uint256) { return x.length; }
}` } }, settings: { evmVersion: 'osaka', outputSelection: { '*': { '': ['ast'], '*': ['abi', 'storageLayout', 'evm.methodIdentifiers'] } } } }
const output = compile(input)
const source = new MidnightSource({ input, output }, { entrySource: 'Probe.sol', contract: 'Probe' })
const lowering = new MidnightLowering(source, { abi, abiRequire: (condition, ctx) => ctx.emit(`.require ${condition} "ABI"`) })
const declarations = source.callGraph().map(r => source.declaration(r.declaration))
const codes = new Map(declarations.map(d => [d.name, lowering.compileFunction(d)]))
const param = name => declarations.find(d => d.name === name).parameters.parameters
const binding = (p, expr) => `.letVar "local_${p.id}" ${expr}`
for (const name of ['nested', 'raw', 'dynamicTuple', 'fixedDynamic']) {
 const p = param(name)[0]
 assert(codes.get(name).includes(`.assignVar "local_${p.id}" (.add (.literal 4) (.calldataload (.literal 4)))`))
 assert(codes.get(name).includes(binding(p, '(.literal 0)')))
}
assert(codes.get('fixedWords').includes(binding(param('fixedWords')[1], '(.literal 36)')))
assert(codes.get('staticTuple').includes(binding(param('staticTuple')[2], '(.literal 100)')))
const hp = param('helper')[0]
assert(codes.get('helper').includes(binding(hp, `(.param "arg_${hp.id}")`)))
assert.doesNotMatch(codes.get('helper'), /paramDynamicDataOffset/)
assert.match(codes.get('materialize'), /calldatacopy/)
assert.match(codes.get('materialize'), /unsafeBlock/)
assert.match(codes.get('materialize'), /forEach/)
// Actual pinned source has bytes[], bytes, and dynamic struct public parameters.
// Exercise their exact source-type bindings too (isolated empty probe bodies).
const pinned = new MidnightSource(compilePinnedSource(root, config), config)
const actual = new MidnightLowering(pinned, { abi })
const actualCodes = ['multicall', 'take', 'toId'].map(name => {
 const d = structuredClone(pinned.declaration(pinned.callGraph().find(r => pinned.declaration(r.declaration).name === name).declaration))
 d.body = { nodeType: 'Block', statements: [] }; d.returnParameters.parameters = []
 return actual.compileFunction(d)
})
const functions = [...codes.values(), ...actualCodes]
const selectors = [...source.surface().filter(r => r.abi.type === 'function').map(r => BigInt('0x' + r.selector).toString()),
 ...['multicall', 'take', 'toId'].map(name => BigInt('0x' + pinned.surface().find(r => r.abi.type === 'function' && r.abi.name === name).selector).toString())]
// Selector list follows CM public-function order, not source.surface sorting.
const selectorByName = new Map([...source.surface(), ...pinned.surface()].filter(r => r.abi.type === 'function').map(r => [r.abi.name, BigInt('0x' + r.selector).toString()]))
const orderedSelectors = functions.filter(f => !f.includes('isInternal := true')).map(f => selectorByName.get(/^\{ name := "([^"]+)"/.exec(f)[1]))
assert.equal(orderedSelectors.length, selectors.length)
const dir = mkdtempSync(join(tmpdir(), 'midnight-public-composites-'))
try {
 const yulPath = join(dir, 'Probe.yul'), leanPath = join(dir, 'Probe.lean')
 const lean = `import Compiler.CompilationModel
import Compiler.CodegenCommon
import Compiler.Yul.PrettyPrint
open Compiler.CompilationModel
set_option maxRecDepth 100000
set_option maxHeartbeats 0
${functions.map((f, i) => `def probe_${i} : FunctionSpec := ${f.replaceAll('\n', ' ')}`).join('\n')}
def spec : CompilationModel := { name := "Probe", fields := [], constructor := none, errors := [{ name := "Panic", params := [.uint256] }], functions := [${functions.map((_, i) => `probe_${i}`).join(',')}] }
#eval do
 match compile spec [${orderedSelectors.join(',')}] .osaka with
 | .error err => throw (IO.userError err)
 | .ok ir =>
   IO.FS.writeFile ${JSON.stringify(yulPath)} (Compiler.Yul.render (Compiler.CodegenCommon.emitYul ir))
   IO.println "PUBLIC_COMPOSITE_YUL_OK"
`
 writeFileSync(leanPath, lean)
 const log = run('lake', ['env', 'lean', '-s', '65536', leanPath], undefined, 400000)
 assert.match(log, /PUBLIC_COMPOSITE_YUL_OK/)
 const yul = readFileSync(yulPath, 'utf8')
 const result = compile({ language: 'Yul', sources: { 'Probe.yul': { content: yul } }, settings: { evmVersion: 'osaka', optimizer: { enabled: true }, outputSelection: { '*': { '*': ['evm.bytecode.object'] } } } })
 const bytecode = result.contracts['Probe.yul'].Probe.evm.bytecode.object
 assert(bytecode.length > 0)
 // Negative control reproduces the original native-scope-versus-Yul mismatch.
 const p = param('nested')[0]
 const broken = yul.replace(`local_${p.id} := add(4, calldataload(4))`, `local_${p.id} := arg_${p.id}`)
 assert.notEqual(broken, yul)
 const bad = JSON.parse(run(solc, ['--standard-json'], JSON.stringify({ language: 'Yul', sources: { 'Broken.yul': { content: broken } }, settings: { evmVersion: 'osaka', outputSelection: { '*': { '*': ['evm.bytecode.object'] } } } })))
 assert((bad.errors ?? []).some(e => e.severity === 'error' && e.message.includes(`arg_${p.id}`)))
 console.log(`PASS: ${functions.length} typed probes; Lean -> Yul -> pinned solc (${bytecode.length / 2} byte deployment); original undefined-param negative control rejected. Includes memory materialization/capture and unchanged internal pointer.`)
} catch (error) {
 console.error(`Probe artifacts retained at ${dir}`)
 throw error
}
rmSync(dir, { recursive: true, force: true })
