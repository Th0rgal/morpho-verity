/** Source-derived builtin and inline-Yul lowering. No source-function templates.
 * Exports lowerCall(node,ctx), lowerMember(node,ctx), lowerAssembly(node,ctx),
 * supportDefinitions(). ctx: source, eval, emit, fresh, bind, abiRequire, fail,
 * readDeclaration, writeDeclaration. bind returns Lean Expr text; emit accepts
 * Lean Stmt text. emit must be replaceable (branch/loop capture).
 * Value uses frontend types and scalar/memory/calldata/literal places. Multiple
 * results additionally use {type:{kind:'tuple',elements:[types]},place:'tuple',
 * values:[Value]}; zero returns are an empty tuple. Core must unpack these.
 * Composite memory is an ABI image, not Solidity's pointer-based heap layout.
 * Free memory pointer 0x40 must be initialized by core. ABI guards revert.
 * Only unrecognized categories return undefined; recognized unsupported input
 * throws. Helpers target pinned Verity 760e9feb (YulStmt.expr, not exprStmt).
 */
import * as ABI from './midnight-abi.mjs'
import { keccak256 } from './keccak256.mjs'
const E = (op,...xs) => `(.${op}${xs.length ? ' '+xs.join(' ') : ''})`
const L = n => E('literal',String(n)), Z=L(0), W=L(32)
const add=(a,b)=>E('add',a,b)
const BYTES={kind:'bytes',abi:'bytes'}
const UINT={kind:'scalar',abi:'uint256',bits:256,signed:false}
const BOOL={kind:'scalar',abi:'bool',bits:8,signed:false}
const scalar=(expr,type=UINT)=>({type,place:'scalar',expr})
const tuple=values=>({type:{kind:'tuple',elements:values.map(v=>v.type)},place:'tuple',values})
function fail(ctx,why,node) { ctx.fail?.(why,node); throw new Error(why) }
function need(ctx,test,why,node) { if(!test) fail(ctx,why,node) }
function expr(v,ctx,node) { need(ctx,v?.place==='scalar' && v.expr,'expected scalar value',node); return v.expr }
function capture(ctx,fn) {
  if(typeof ctx.capture==='function')return `[${ctx.capture(fn).body.join(',\n')}]`
  const stmts=[],old=ctx.emit;ctx.emit=s=>stmts.push(s)
  try{fn()}finally{ctx.emit=old}
  return `[${stmts.join(',\n')}]`
}
const opcodes={
  gas:{argc:0,result:true,reads:true,writes:false},
  clz:{argc:1,result:true,reads:false,writes:false},
  create2:{argc:4,result:true,reads:true,writes:true},
  extcodecopy:{argc:4,result:false,reads:true,writes:false},
  revert:{argc:2,result:false,reads:false,writes:false},
}
function opcode(op,args,ctx,node) {
  const spec=opcodes[op]; need(ctx,spec && args.length===spec.argc,`invalid opcode ${op} arity`,node)
  if(op==='gas'||op==='clz') {
    // Consumer-owned intrinsic: native gas/clz semantics remain assumed, not
    // provided by Verity. Cancun is its earliest fork enum; CLZ requires Osaka.
    const lowering = op==='clz' ? '(.verbatim 1 1 "1e")' : '(.builtin "gas")'
    return ctx.bind(E('intrinsic',JSON.stringify(op),lowering,op==='clz'?'.osaka':'.cancun',`[${args.join(', ')}]`),`yul_${op}`)
  }
  if(op==='revert') {
    // Freeze operands before entering the raw fragment, including memory loads.
    // These locals must stay visible across the core's unsafe-block wrappers.
    const names=args.map((arg,i)=>{const name=ctx.fresh(i?'revert_size':'revert_offset');ctx.emit(`.letVar ${JSON.stringify(name)} ${arg}`);return name})
    ctx.emit(`.unsafeYul (Compiler.CompilationModel.UnsafeYulFragment.rawRevert (Compiler.Yul.YulExpr.ident ${JSON.stringify(names[0])}) (Compiler.Yul.YulExpr.ident ${JSON.stringify(names[1])}) { name := ${JSON.stringify(ctx.fresh('raw_revert_exact_payload'))}, obligation := "Assumed native EVM revert: terminates execution and reverts with exactly the frozen offset/size memory slice; no external interaction.", proofStatus := .assumed })`)
    return null
  }
  const name=spec.result?ctx.fresh(`yul_${op}`):null
  ctx.emit(`.ecm (midnightOpcode_${op}${name?' '+JSON.stringify(name):''}) [${args.join(', ')}]`)
  return name?E('localVar',JSON.stringify(name)):null
}
/** Tiny per-opcode boundaries. Assumptions are explicit and not proofs of
 * interpreter/EVM correspondence. create2 changes external state; extcodecopy
 * changes memory (not storage); raw revert terminates with exact memory bytes. */
export function supportDefinitions() {
  return Object.entries(opcodes).map(([op,s])=>`/-- Assumed native EVM ${op} opcode semantics, including memory, gas, failure and external-world effects where applicable. -/
def midnightOpcode_${op}${s.result?' (resultVar : String)':''} : Compiler.ECM.ExternalCallModule where
  name := "source.opcode.${op}"
  numArgs := ${s.argc}
  resultVars := ${s.result?'[resultVar]':'[]'}
  writesState := ${s.writes}
  readsState := ${s.reads}
  proofStatus := .assumed
  axioms := ["evm_${op}_exact_opcode_effects"]
  compile := fun _ctx args =>
    if args.length == ${s.argc} then
      pure [${s.result?'Compiler.Yul.YulStmt.let_ resultVar':'Compiler.Yul.YulStmt.expr'} (Compiler.Yul.YulExpr.call "${op}" args)]
    else throw "${op}: wrong argument count"
`).join('\n')
}
function allocBytes(n,ctx) {
  const ptr=ctx.bind(E('mload',L(64)),'bytes_ptr')
  const size=ctx.bind(add(n,L(63)),'bytes_alloc_size')
  ctx.abiRequire(E('ge',size,n))
  const end=ctx.bind(add(ptr,E('bitAnd',size,L((1n<<256n)-32n))),'bytes_end')
  ctx.abiRequire(E('ge',end,ptr))
  ctx.emit(`.mstore ${L(64)} ${end}`);ctx.emit(`.mstore ${ptr} ${n}`)
  return {type:BYTES,place:'memory',ptr}
}
function bytes(v,ctx) { need(ctx,['bytes','string'].includes(v?.type?.kind),'expected byte view'); return ABI.encodePacked([v],ctx) }
function returnData(ctx) {
  const n=ctx.bind(E('returndataSize'),'returndata_size'),v=allocBytes(n,ctx)
  ctx.emit(`.returndataCopy ${add(v.ptr,W)} ${Z} ${n}`);return v
}
function selector(decl,ctx,node) {
  need(ctx,decl?.nodeType==='FunctionDefinition','expected referenced function definition',node)
  const sig=`${decl.name}(${decl.parameters.parameters.map(p=>ctx.source.abiType(ctx.source.type(p))).join(',')})`
  const hex=keccak256(Buffer.from(sig)).slice(0,8)
  need(ctx,decl.functionSelector?.toLowerCase()===hex,`function selector/type signature mismatch: ${sig}`,node)
  return {type:{kind:'scalar',abi:'bytes4',bits:32,signed:false,alignment:'left'},place:'scalar',expr:L(BigInt('0x'+hex)<<224n)}
}
function typedArgs(decl,nodes,ctx,node) {
  const ps=decl.parameters.parameters;need(ctx,nodes.length===ps.length,'external argument arity mismatch',node)
  return nodes.map((n,i)=>{need(ctx,n,'tuple argument hole',node);const v=ctx.eval(n),type=ctx.source.type(ps[i]);
    // Solidity's typed AST already establishes implicit conversion validity;
    // encoder cleans to the declared width/alignment, never inferred names.
    return {...v,type}
  })
}
function calldataFor(decl,nodes,ctx,node) {
  const sel=selector(decl,ctx,node),encoded=ABI.encodeTuple(typedArgs(decl,nodes,ctx,node),ctx)
  return ABI.encodePacked([sel,encoded],ctx)
}
/** Eager, bounded ABI validation, then zero-copy memory views. */
function decode(types,input,ctx,node) {
  const v=bytes(input,ctx),base=ABI.byteData(v,ctx),length=ABI.byteLength(v,ctx)
  const checked=(off,n)=>{ctx.abiRequire(E('le',off,length));ctx.abiRequire(E('le',n,E('sub',length,off)))}
  const load=off=>{checked(off,W);return ctx.bind(E('mload',add(base,off)),'decode_word')}
  function item(t,parent,slot) {
    let off=slot
    if(ABI.isDynamic(t)) { const relative=load(slot); off=ctx.bind(add(parent,relative),'decode_offset');ctx.abiRequire(E('ge',off,parent)) }
    checked(off,L(ABI.isDynamic(t)?32:ABI.headBytes(t)))
    if(t.kind==='scalar') { const raw=load(off),clean=ABI.cleanScalar(t,raw,ctx);ctx.abiRequire(E('eq',raw,clean));return scalar(raw,t) }
    if(t.kind==='bytes'||t.kind==='string') {const n=load(off);checked(add(off,W),n)}
    else if(t.kind==='struct') { let cursor=0;for(const m of t.members) {item(m.type,off,add(off,L(cursor)));cursor+=ABI.headBytes(m.type)} }
    else if(t.kind==='array') {
      const dynamic=t.length===null,n=dynamic?load(off):L(t.length),start=dynamic?add(off,W):off,stride=ABI.headBytes(t.element)
      checked(start,Z);ctx.abiRequire(E('le',n,E('div',E('sub',length,start),L(stride))))
      const name=ctx.fresh('decode_i'),i=E('localVar',JSON.stringify(name))
      const body=capture(ctx,()=>item(t.element,start,add(start,E('mul',i,L(stride)))))
      ctx.emit(`.forEach ${JSON.stringify(name)} ${n} ${body}`)
    } else fail(ctx,`unsupported decoded type ${t.kind}`,node)
    return {type:t,place:'memory',ptr:add(base,off)}
  }
  checked(Z,L(types.reduce((s,t)=>s+ABI.headBytes(t),0)))
  let cursor=0;const values=types.map(t=>{const r=item(t,Z,L(cursor));cursor+=ABI.headBytes(t);return r})
  return values.length===1?values[0]:tuple(values)
}
function decodeTypes(node,ctx) {
  const ns=node.nodeType==='TupleExpression'?node.components:[node]
  return ns.map(n=>{need(ctx,n,'decode type tuple hole',node)
    if(n.nodeType==='ElementaryTypeNameExpression')return ctx.source.type(n.typeName)
    if(['ElementaryTypeName','UserDefinedTypeName','ArrayTypeName'].includes(n.nodeType))return ctx.source.type(n)
    if(n.nodeType==='Identifier'||n.nodeType==='IdentifierPath'||n.nodeType==='MemberAccess') {
      const d=ctx.source.resolve(n)
      need(ctx,['StructDefinition','ContractDefinition','UserDefinedValueTypeDefinition'].includes(d.nodeType),'abi.decode requires a type reference',n)
      return ctx.source.type({nodeType:'UserDefinedTypeName',referencedDeclaration:d.id,id:`builtin-type-${d.id}`})
    }
    fail(ctx,'unsupported ABI decode typename AST',n)
  })
}
export function lowerCall(node,ctx) {
  if(node?.nodeType!=='FunctionCall'||node.kind==='typeConversion')return undefined
  const e=node.expression,args=node.arguments??[]
  if(e.nodeType==='MemberAccess'&&e.expression?.nodeType==='Identifier'&&e.expression.name==='abi'&&e.expression.referencedDeclaration<0) {
    switch(e.memberName) {
      case 'encode':return ABI.encodeTuple(args.map(a=>ctx.eval(a)),ctx)
      case 'encodePacked':return ABI.encodePacked(args.map(a=>ctx.eval(a)),ctx)
      case 'encodeCall': {
        need(ctx,args.length===2,'abi.encodeCall arity',node)
        const decl=ctx.source.resolve(args[0],'FunctionDefinition')
        const actual=args[1].nodeType==='TupleExpression'?args[1].components:[args[1]]
        return calldataFor(decl,actual,ctx,node)
      }
      case 'decode':need(ctx,args.length===2,'abi.decode arity',node);return decode(decodeTypes(args[1],ctx),ctx.eval(args[0]),ctx,node)
      default:fail(ctx,`unsupported ABI builtin ${e.memberName}`,node)
    }
  }
  if(e.nodeType==='Identifier'&&e.name==='keccak256'&&e.referencedDeclaration<0) {
    need(ctx,args.length===1,'keccak256 arity',node);const v=bytes(ctx.eval(args[0]),ctx)
    return scalar(ctx.bind(E('keccak256',ABI.byteData(v,ctx),ABI.byteLength(v,ctx)),'keccak'),{kind:'scalar',abi:'bytes32',bits:256,signed:false,alignment:'left'})
  }
  if(e.nodeType!=='MemberAccess')return undefined
  if(['call','delegatecall','staticcall'].includes(e.memberName)&&!(e.referencedDeclaration>=0)) {
    need(ctx,/^address(?: payable)?$/.test(e.expression.typeDescriptions?.typeString??''),'low-level call on non-address',node)
    need(ctx,args.length===1,'low-level call arity/options unsupported',node)
    const target=expr(ctx.eval(e.expression),ctx,node),payload=bytes(ctx.eval(args[0]),ctx),gas=opcode('gas',[],ctx,node)
    const operands=[gas,target,...(e.memberName==='call'?[Z]:[]),ABI.byteData(payload,ctx),ABI.byteLength(payload,ctx),Z,Z]
    const success=ctx.bind(E(e.memberName,...operands),'call_success'),data=returnData(ctx)
    return tuple([scalar(success,BOOL),data])
  }
  if(e.referencedDeclaration>=0) {
    const decl=ctx.source.resolve(e)
    if(decl.nodeType!=='FunctionDefinition'||decl.body)return undefined
    need(ctx,['external','public'].includes(decl.visibility),'nonexternal bodyless function',node)
    const target=expr(ctx.eval(e.expression),ctx,node),payload=calldataFor(decl,args,ctx,node)
    const types=decl.returnParameters.parameters.map(p=>ctx.source.type(p))
    // No-return calls must reject a missing target BEFORE making the call.
    if(!types.length) ctx.abiRequire(E('gt',E('extcodesize',target),Z))
    // A typed call only needs its statically declared output, not the callee's
    // arbitrary trailing returndata. Dynamic returns require a different decoder
    // and are deliberately rejected rather than guessing an output bound.
    need(ctx,types.every(t=>!ABI.isDynamic(t)),'dynamic high-level external returns unsupported',node)
    const outputSize=L(types.reduce((n,t)=>n+ABI.headBytes(t),0))
    const output=types.length?ABI.byteData(allocBytes(outputSize,ctx),ctx):Z
    const gas=opcode('gas',[],ctx,node)
    const staticCall=['view','pure'].includes(decl.stateMutability)
    const success=ctx.bind(E(staticCall?'staticcall':'call',gas,target,...(staticCall?[]:[Z]),ABI.byteData(payload,ctx),ABI.byteLength(payload,ctx),output,outputSize),'external_success')
    // Only failure bubbles the full returndata. Keep allocation and copying in
    // the captured branch (including core's statement-local unsafe annotations).
    const failure=capture(ctx,()=>{
      const data=returnData(ctx)
      opcode('revert',[ABI.byteData(data,ctx),ABI.byteLength(data,ctx)],ctx,node)
    })
    ctx.emit(`.ite ${E('logicalNot',success)} ${failure} []`)
    if(!types.length) return tuple([])
    ctx.abiRequire(E('ge',E('returndataSize'),outputSize))
    function staticResult(t,ptr) {
      if(t.kind==='scalar') {
        const raw=ctx.bind(E('mload',ptr),'external_word')
        ctx.abiRequire(E('eq',raw,ABI.cleanScalar(t,raw,ctx)))
        return scalar(raw,t)
      }
      if(t.kind==='struct') {
        let offset=0
        for(const m of t.members) {staticResult(m.type,add(ptr,L(offset)));offset+=ABI.headBytes(m.type)}
      } else if(t.kind==='array') {
        const name=ctx.fresh('external_i'),i=E('localVar',JSON.stringify(name))
        const body=capture(ctx,()=>staticResult(t.element,add(ptr,E('mul',i,L(ABI.headBytes(t.element))))))
        ctx.emit(`.forEach ${JSON.stringify(name)} ${L(t.length)} ${body}`)
      } else fail(ctx,`unsupported static external return ${t.kind}`,node)
      return {type:t,place:'memory',ptr}
    }
    let offset=0
    const values=types.map(t=>{const v=staticResult(t,add(output,L(offset)));offset+=ABI.headBytes(t);return v})
    return values.length===1?values[0]:tuple(values)
  }
  return undefined
}
export function lowerMember(node,ctx) {
  if(node?.nodeType!=='MemberAccess'||node.memberName!=='code'||node.referencedDeclaration>=0)return undefined
  need(ctx,/^address(?: payable)?$/.test(node.expression.typeDescriptions?.typeString??''),'address.code requires typed address',node)
  const target=expr(ctx.eval(node.expression),ctx,node),n=ctx.bind(E('extcodesize',target),'code_size'),v=allocBytes(n,ctx)
  opcode('extcodecopy',[target,add(v.ptr,W),Z,n],ctx,node);return v
}
const natives={add:['add',2],sub:['sub',2],mul:['mul',2],div:['div',2],sdiv:['sdiv',2],mod:['mod',2],smod:['smod',2],and:['bitAnd',2],or:['bitOr',2],xor:['bitXor',2],not:['bitNot',1],iszero:['logicalNot',1],eq:['eq',2],lt:['lt',2],gt:['gt',2],slt:['slt',2],sgt:['sgt',2],shl:['shl',2],shr:['shr',2],sar:['sar',2],byte:['byte',2],signextend:['signextend',2],mload:['mload',1],tload:['tload',1],keccak256:['keccak256',2],extcodesize:['extcodesize',1],returndatasize:['returndataSize',0],calldataload:['calldataload',1],calldatasize:['calldatasize',0],call:['call',7],staticcall:['staticcall',6],delegatecall:['delegatecall',6]}
const statements={mstore:2,tstore:2,calldatacopy:3,returndatacopy:3}
export function lowerAssembly(node,ctx) {
  if(node?.nodeType!=='InlineAssembly')return undefined
  need(ctx,node.AST?.nodeType==='YulBlock','inline assembly missing actual Yul AST',node)
  const refs=new Map(),locals=new Map()
  for(const r of node.externalReferences??[]) {need(ctx,r.src&&!refs.has(r.src)&&Number.isInteger(r.declaration),'ambiguous assembly external reference',node);refs.set(r.src,r)}
  function reference(n) {const r=refs.get(n.src);if(r)need(ctx,!r.isSlot&&!r.isOffset&&!r.suffix,'assembly storage/calldata suffix unsupported',n);return r}
  function val(n) {
    if(n.nodeType==='YulLiteral') {need(ctx,n.kind==='number'||n.kind==='bool','Yul literal kind unsupported',n);return L(n.kind==='bool'?(n.value==='true'?1:0):BigInt(n.value))}
    if(n.nodeType==='YulIdentifier') {
      const r=reference(n)
      if(r) {const v=ctx.readDeclaration(r.declaration);need(ctx,v.place==='scalar'||v.place==='memory','assembly reference must be scalar/memory',n);return v.place==='scalar'?v.expr:v.ptr}
      need(ctx,locals.has(n.name),'unresolved Yul identifier (reference must match src span)',n);return E('localVar',JSON.stringify(locals.get(n.name)))
    }
    need(ctx,n.nodeType==='YulFunctionCall','unsupported Yul expression',n)
    const op=n.functionName.name,ns=n.arguments??[],spec=natives[op]??(opcodes[op]?.result?[op,opcodes[op].argc]:null)
    need(ctx,spec&&ns.length===spec[1],`unsupported Yul expression opcode/arity ${op}`,n)
    // EVM Yul evaluates arguments right-to-left. Bind all reads before writes.
    const xs=new Array(ns.length);for(let i=ns.length-1;i>=0;i--)xs[i]=ctx.bind(val(ns[i]),'yul_arg')
    return opcodes[op]?opcode(op,xs,ctx,n):ctx.bind(E(spec[0],...xs),'yul_value')
  }
  function stmt(n) {
    if(n.nodeType==='YulBlock') {const before=new Map(locals);for(const s of n.statements)stmt(s);locals.clear();for(const p of before)locals.set(...p);return}
    if(n.nodeType==='YulAssignment') {
      need(ctx,n.variableNames.length===1,'multi-assignment Yul unsupported',n)
      const rhs=ctx.bind(val(n.value),'yul_rhs'),lhs=n.variableNames[0],r=reference(lhs)
      if(r) {const old=ctx.readDeclaration(r.declaration);need(ctx,['scalar','memory'].includes(old.place),'unsupported assembly assignment location',lhs);ctx.writeDeclaration(r.declaration,{...old,...(old.place==='scalar'?{expr:rhs}:{ptr:rhs})})}
      else {need(ctx,locals.has(lhs.name),'unresolved Yul assignment',lhs);ctx.emit(`.assignVar ${JSON.stringify(locals.get(lhs.name))} ${rhs}`)}
      return
    }
    if(n.nodeType==='YulVariableDeclaration') {need(ctx,n.variables.length===1,'multi-variable Yul declaration unsupported',n);const rhs=n.value?val(n.value):Z,name=ctx.fresh('yul_'+n.variables[0].name);locals.set(n.variables[0].name,name);ctx.emit(`.letVar ${JSON.stringify(name)} ${rhs}`);return}
    if(n.nodeType==='YulExpressionStatement') {
      const call=n.expression;need(ctx,call.nodeType==='YulFunctionCall','noncall Yul expression statement',n)
      const op=call.functionName.name,ns=call.arguments??[]
      const arity=statements[op]??(opcodes[op]&&!opcodes[op].result?opcodes[op].argc:undefined)
      need(ctx,arity!==undefined&&ns.length===arity,`unsupported Yul statement opcode ${op}`,n)
      const xs=new Array(ns.length);for(let i=ns.length-1;i>=0;i--)xs[i]=ctx.bind(val(ns[i]),'yul_arg')
      if(opcodes[op])opcode(op,xs,ctx,n)
      else ctx.emit(`.${op==='returndatacopy'?'returndataCopy':op} ${xs.join(' ')}`)
      return
    }
    fail(ctx,`unsupported Yul statement ${n.nodeType}`,n)
  }
  stmt(node.AST);return true
}
