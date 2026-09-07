#!/usr/bin/env node
/** Builtin-specific tests; uses real solc typed AST, never modifies Solidity. */
import assert from 'node:assert/strict'
import {spawnSync} from 'node:child_process'
import {readFileSync,readdirSync,mkdtempSync,writeFileSync,rmSync} from 'node:fs'
import {tmpdir} from 'node:os'
import {resolve} from 'node:path'
import {MidnightSource,walk} from './lib/midnight-source.mjs'
import {lowerCall,lowerMember,lowerAssembly,supportDefinitions} from './lib/midnight-builtins.mjs'
const root=resolve(import.meta.dirname,'..'),sr=resolve(root,'morpho-midnight'),sources={}
function collect(dir) {for(const e of readdirSync(resolve(sr,dir),{withFileTypes:true})) {const p=dir+'/'+e.name;if(e.isDirectory())collect(p);else if(p.endsWith('.sol'))sources[p]={content:readFileSync(resolve(sr,p),'utf8')}}}
collect('src')
const input={language:'Solidity',sources,settings:{evmVersion:'osaka',outputSelection:{'*':{'':['ast'],'*':['abi','storageLayout','evm.methodIdentifiers']}}}}
const run=spawnSync(process.env.MORPHO_SOLC_0_8_34??'/home/claudine/solidity_to_verity/tools/bin/solc-0.8.34-linux-amd64',['--standard-json'],{input:JSON.stringify(input),encoding:'utf8',maxBuffer:100_000_000})
assert.equal(run.status,0,run.stderr);const output=JSON.parse(run.stdout)
assert.equal((output.errors??[]).filter(e=>e.severity==='error').length,0,JSON.stringify(output.errors))
const source=new MidnightSource({input,output},{entrySource:'src/Midnight.sol',contract:'Midnight'})
const U={kind:'scalar',abi:'uint256',bits:256,signed:false},B={kind:'bytes',abi:'bytes'}
function context() {
 const c={source,stmts:[],counter:0,values:new Map(),emit(s){this.stmts.push(s)},fresh(h){return h+'_'+this.counter++},bind(e,h){const n=this.fresh(h);this.emit(`.letVar ${JSON.stringify(n)} ${e}`);return `(.localVar ${JSON.stringify(n)})`},abiRequire(e){this.emit(`.ite ${e} [] [.unsafeYul (Compiler.CompilationModel.UnsafeYulFragment.rawRevert (.lit 0) (.lit 0) { name := "test_abi_failure", obligation := "Assumed empty-payload terminal revert", proofStatus := .assumed })]`)},fail(m){throw new Error(m)},readDeclaration(id){if(!this.values.has(id)){const d=source.declaration(id),type=source.type(d);this.values.set(id,type.kind==='scalar'?{type,place:'scalar',expr:'(.literal 1)'}:{type,place:'memory',ptr:'(.literal 128)'})}return this.values.get(id)},writeDeclaration(id,v){this.values.set(id,v)},eval(n){
   const b=lowerCall(n,this)??lowerMember(n,this);if(b!==undefined)return b
   if(n.nodeType==='Identifier'&&n.referencedDeclaration>=0)return this.readDeclaration(n.referencedDeclaration)
   if(n.nodeType==='Literal'&&['string','hexString','unicodeString'].includes(n.kind))return {type:B,place:'literal',hex:n.hexValue}
   if(n.nodeType==='FunctionCall'&&n.kind==='typeConversion')return {type:{...U,abi:'address',bits:160},place:'scalar',expr:'(.literal 123)'}
   throw new Error('test eval unsupported '+n.nodeType+':'+n.src)
 }};return c
}
const leanBodies=[]
let assemblies=0,calls=0,codes=0;const assemblyOps=new Set()
for(const [file,src] of Object.entries(output.sources)) {
 if(!file.startsWith('src/libraries/')&&file!=='src/Midnight.sol')continue
 walk(src.ast,n=>{
  if(n.nodeType==='InlineAssembly') {const c=context();assert.equal(lowerAssembly(n,c),true);assert.ok(c.stmts.length);leanBodies.push(c.stmts);assemblies++;walk(n.AST,x=>{if(x.nodeType==='YulFunctionCall')assemblyOps.add(x.functionName.name)})
    const bad=structuredClone(n);const r=bad.externalReferences[0];if(r){r.src='99999:1:0';assert.throws(()=>lowerAssembly(bad,context()),/unresolved Yul/)}
  }
  if(n.nodeType==='FunctionCall'&&n.expression.nodeType==='MemberAccess'&&n.expression.expression.name==='abi'&&n.expression.memberName==='encodeCall') {
    const c=context(),v=lowerCall(n,c);assert.equal(v.place,'memory');assert.ok(c.stmts.length);calls++
    const d=source.resolve(n.arguments[0]),old=d.functionSelector;d.functionSelector='deadbeef';assert.throws(()=>lowerCall(n,context()),/selector\/type signature mismatch/);d.functionSelector=old
  }
  if(n.nodeType==='MemberAccess'&&n.memberName==='code') {const c=context(),v=lowerMember(n,c);assert.equal(v.place,'memory');assert.match(c.stmts.join('\n'),/midnightOpcode_extcodecopy/);codes++}
 })
}
const transferNodes=[];walk(output.sources['src/libraries/SafeTransferLib.sol'].ast,n=>{if(n.nodeType==='FunctionCall'&&n.expression.memberName==='call')transferNodes.push(n)})
for(const n of transferNodes){const c=context(),v=lowerCall(n,c);assert.equal(v.place,'tuple');assert.equal(v.values.length,2);assert.match(c.stmts.join('\n'),/\.call /);assert.match(c.stmts.join('\n'),/returndataCopy/)}
const decodeNodes=[];walk(output.sources['src/Midnight.sol'].ast,n=>{if(n.nodeType==='FunctionCall'&&n.expression.memberName==='decode')decodeNodes.push(n)})
for(const n of decodeNodes){const c=context(),v=lowerCall(n,c);assert.equal(v.type.kind,'struct');assert.match(c.stmts.join('\n'),/decode_i/)}
for(const src of ['src/libraries/SafeTransferLib.sol'])walk(output.sources[src].ast,n=>{if(n.nodeType==='FunctionCall'&&n.expression.memberName==='decode'){const c=context(),v=lowerCall(n,c);assert.equal(v.type.abi,'bool');assert.match(c.stmts.join('\n'),/logicalNot/);assert.match(c.stmts.join('\n'),/\.eq /)}})
assert.equal(lowerCall({nodeType:'Identifier'},context()),undefined)
assert.throws(()=>lowerAssembly({nodeType:'InlineAssembly',AST:{nodeType:'YulBlock',statements:[{nodeType:'YulIf'}]}},context()),/unsupported Yul statement/)
assert.match(supportDefinitions(),/writesState := true/);assert.match(supportDefinitions(),/evm_revert_exact_opcode_effects/)
let interfaceCalls=0,unsupportedInterfaceShapes=0
for(const d of source.nodes.values()) {
 if(d.nodeType!=='FunctionDefinition'||d.body||!d.functionSelector||!['external','public'].includes(d.visibility))continue
 const n={nodeType:'FunctionCall',kind:'functionCall',expression:{nodeType:'MemberAccess',referencedDeclaration:d.id,expression:{nodeType:'FunctionCall',kind:'typeConversion'}},arguments:d.parameters.parameters.map(p=>({nodeType:'Identifier',referencedDeclaration:p.id}))}
 const c=context()
 // Exercise real interface declarations; packed/ABI unsupported shapes fail
 // separately in ABI tests, rather than replacing them with made-up values.
 try {lowerCall(n,c)} catch(e) {if(/arrays with dynamic elements|unsupported user-defined type EnumDefinition|dynamic high-level external returns unsupported/.test(e.message)){unsupportedInterfaceShapes++;continue}throw e}
 assert.match(c.stmts.join('\n'),['view','pure'].includes(d.stateMutability)?/\.staticcall /:/\.call /)
 assert.match(c.stmts.join('\n'),/UnsafeYulFragment.rawRevert/);assert.doesNotMatch(c.stmts.join('\n'),/midnightOpcode_(?:gas|clz|revert)/)
 leanBodies.push(c.stmts);interfaceCalls++
}
assert.ok(interfaceCalls>0)
if(process.argv.includes('--lean')) {
 const dir=mkdtempSync(resolve(tmpdir(),'midnight-builtins-'))
 try {
  const file=resolve(dir,'BuiltinsTest.lean')
  writeFileSync(file,'import Verity.Core.Model.Types\n'+supportDefinitions()+'\n'+leanBodies.map((s,i)=>`def builtinTest${i} : List Compiler.CompilationModel.Stmt := [${s.join(',\n')}]`).join('\n'))
  const r=spawnSync('lake',['env','lean',file],{cwd:root,encoding:'utf8',maxBuffer:10_000_000,timeout:600_000})
  assert.equal(r.status,0,`${r.error ?? ''}\n${r.stdout}\n${r.stderr}`)
  assert(!/PANIC at/.test(r.stdout+r.stderr),'Lean panic is not successful elaboration')
 } finally {rmSync(dir,{recursive:true,force:true})}
}
console.log(JSON.stringify({assemblies,assemblyOps:[...assemblyOps].sort(),encodeCalls:calls,addressCodeMembers:codes,lowLevelCalls:transferNodes.length,marketDecodes:decodeNodes.length,interfaceCalls,unsupportedInterfaceShapes,lean:process.argv.includes('--lean'),status:'PASS'}))
