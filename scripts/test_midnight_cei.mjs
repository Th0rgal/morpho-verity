#!/usr/bin/env node
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { compilePinnedSource, MidnightSource, walk } from './lib/midnight-source.mjs'
import { sourceCEIPolicies } from './lib/midnight-cei.mjs'
const config=JSON.parse(readFileSync('config/midnight-full-import.json','utf8'))
const source=new MidnightSource(compilePinnedSource(process.cwd(),config),config)
const policies=sourceCEIPolicies(source)
assert.equal(policies.size,source.callGraph().length)
const enabled=[...policies.values()].filter(p=>p.allowPostInteractionWrites)
assert(enabled.length>0 && enabled.length<policies.size,'not a blanket CEI override')
for(const p of policies.values()) {
  assert.equal(p.allowPostInteractionWrites,p.witnesses.length>0)
  assert.match(p.claim,/NOT a proof/)
  for(const w of p.witnesses) {
    assert(w.barrier.origin.spanSha256 && w.write.origin.spanSha256)
  }
}
const setter=source.declarations('FunctionDefinition').find(f=>f.name==='setRoleSetter'&&f.body)
assert.equal(policies.get(setter.id).allowPostInteractionWrites,false,'plain administrative setter keeps CEI validation')
const helper=source.callGraph().map(r=>source.declaration(r.declaration)).find(f=>f.stateMutability==='pure'&&f.parameters.parameters.length===1&&f.returnParameters.parameters.length===1&&source.type(f.parameters.parameters[0]).kind==='scalar')
let assignment
walk(setter.body,n=>{if(n.nodeType==='Assignment'&&!assignment)assignment=n})
assert(helper&&assignment)
const original=setter.body
const call={nodeType:'FunctionCall',id:assignment.id,src:assignment.src,kind:'functionCall',expression:{nodeType:'Identifier',name:helper.name,referencedDeclaration:helper.id},arguments:[]}
const statement=expression=>({nodeType:'ExpressionStatement',expression})
try {
  setter.body={...original,statements:[statement(call),statement(assignment)]}
  assert.equal(sourceCEIPolicies(source).get(setter.id).allowPostInteractionWrites,true)
  setter.body={...original,statements:[statement(assignment),statement(call)]}
  assert.equal(sourceCEIPolicies(source).get(setter.id).allowPostInteractionWrites,false)
} finally { setter.body=original }
console.log(`PASS: ${policies.size} source CEI policies; ${enabled.length} witnessed conservative exceptions; source-order reversal changes exception; ordinary setter remains checked`)
