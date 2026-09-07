#!/usr/bin/env node
// Source-side precondition for the consumer-owned allocator contract.
// This rejects raw writes, not a complete generated-memory proof.
import { generateFull } from './import_midnight_full.mjs'
import { walk } from './lib/midnight-source.mjs'
const { source } = generateFull()
const forbidden = new Set(['mstore','mstore8','mcopy','calldatacopy','returndatacopy','codecopy','extcodecopy','call','staticcall','delegatecall'])
let assemblies = 0
for (const fn of source.callGraph()) for (const assembly of fn.inlineAssembly) {
  assemblies++
  walk(assembly.ast, node => {
    if (node.nodeType === 'YulFunctionCall' && forbidden.has(node.functionName.name))
      throw Error(`Unreviewed source assembly memory writer ${node.functionName.name} in declaration ${fn.declaration}; allocator policy must be reviewed`)
  })
}
console.log(JSON.stringify({sourceAssembliesChecked: assemblies, arbitraryAssemblyMemoryWrites: 0,
  claim: 'Source-side precondition only; generated ABI allocator semantics remain an explicit assumption'}))
