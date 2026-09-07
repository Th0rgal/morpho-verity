/** Source-order accounting for pinned Verity's conservative CEI gate.
 * This is NOT a CEI safety proof. Internal helpers are potential interactions
 * in that gate, and statement-form helpers are potential writes even when the
 * source marks them pure/view. Preserve the source's order and record a local
 * compilation exception only when its conservative effect sequence needs one.
 */
const empty = () => ({ barriers: [], writes: [], witnesses: [] })
function sequence(a, b) {
  return { barriers: [...a.barriers, ...b.barriers], writes: [...a.writes, ...b.writes],
    witnesses: [...a.witnesses, ...b.witnesses,
      ...(a.barriers.length && b.writes.length ? [{ barrier: a.barriers.at(-1), write: b.writes[0], reason: 'sequential potential write after call barrier' }] : [])] }
}
const seq = xs => xs.reduce(sequence, empty())
function branches(a, b) { return { barriers: [...a.barriers, ...b.barriers], writes: [...a.writes, ...b.writes], witnesses: [...a.witnesses, ...b.witnesses] } }

export function sourceCEIPolicies(source) {
  const policies = new Map()
  function stateLvalue(n) {
    if (!n) return false
    if (n.nodeType === 'TupleExpression') return n.components.some(stateLvalue)
    if (n.nodeType === 'MemberAccess') return stateLvalue(n.expression)
    if (n.nodeType === 'IndexAccess') return stateLvalue(n.baseExpression)
    if (n.nodeType === 'Identifier' && n.referencedDeclaration >= 0) {
      const d = source.declaration(n.referencedDeclaration)
      return d.nodeType === 'VariableDeclaration' && (d.stateVariable || d.storageLocation === 'storage')
    }
    return false
  }
  for (const row of source.callGraph()) {
    const decl = source.declaration(row.declaration)
    function witness(n, reason, extra = {}) {
      return { origin: source.origin(Number.isInteger(n?.id) && n.id >= 0 ? n : decl), reason, ...extra }
    }
    function visit(n, assemblyOwner = null) {
      if (!n || typeof n !== 'object') return empty()
      const child = x => visit(x, assemblyOwner)
      switch (n.nodeType) {
        case 'Block': case 'UncheckedBlock': return seq(n.statements.map(child))
        case 'ExpressionStatement': return child(n.expression)
        case 'VariableDeclarationStatement': return child(n.initialValue)
        case 'Return': return child(n.expression)
        case 'EmitStatement': return seq(n.eventCall.arguments.map(child))
        case 'RevertStatement': return seq(n.errorCall.arguments.map(child))
        case 'IfStatement': return sequence(child(n.condition), branches(child(n.trueBody), child(n.falseBody)))
        case 'Conditional': return sequence(child(n.condition), branches(child(n.trueExpression), child(n.falseExpression)))
        case 'ForStatement': case 'WhileStatement': {
          const cycle = seq([child(n.condition), child(n.body), child(n.loopExpression)])
          if (cycle.barriers.length && cycle.writes.length) cycle.witnesses.push({ barrier: cycle.barriers[0], write: cycle.writes[0], reason: 'loop can revisit a write after an interaction barrier' })
          return sequence(child(n.initializationExpression), cycle)
        }
        case 'Assignment': {
          const evaluation = seq([child(n.leftHandSide), child(n.rightHandSide)])
          return sequence(evaluation, stateLvalue(n.leftHandSide) ? { ...empty(), writes: [witness(n, 'source storage assignment')] } : empty())
        }
        case 'UnaryOperation': return sequence(child(n.subExpression), ['++', '--', 'delete'].includes(n.operator) && stateLvalue(n.subExpression) ? { ...empty(), writes: [witness(n, 'source storage unary write')] } : empty())
        case 'FunctionCall': {
          const evaluation = seq([child(n.expression), ...n.arguments.map(child)])
          if (n.kind !== 'functionCall') return evaluation
          const target = n.expression.referencedDeclaration
          let effect = empty()
          if (Number.isInteger(target) && target >= 0) {
            const callee = source.declaration(target)
            if (callee.nodeType === 'FunctionDefinition') {
              effect.barriers.push(witness(n, callee.body ? 'pinned backend conservatively treats internal helpers as interaction barriers' : 'source interface call', { target }))
              if (callee.body && (callee.returnParameters.parameters.length !== 1 || !['pure', 'view'].includes(callee.stateMutability))) {
                effect.writes.push(witness(n, 'statement-form or state-writable source helper is a potential write', { target }))
              }
            }
          } else if (n.expression.nodeType === 'MemberAccess' && ['call', 'delegatecall', 'staticcall'].includes(n.expression.memberName)) {
            effect.barriers.push(witness(n, 'source low-level external call', { opcode: n.expression.memberName }))
          }
          return sequence(evaluation, effect)
        }
        case 'InlineAssembly': return visit(n.AST, n)
        case 'YulBlock': return seq(n.statements.map(x => visit(x, assemblyOwner)))
        case 'YulFunctionCall': {
          const evaluation = seq(n.arguments.map(x => visit(x, assemblyOwner)))
          const op = n.functionName.name, effect = empty()
          const origin = { origin: source.origin(assemblyOwner ?? decl), yulSpan: n.src, opcode: op }
          if (['call', 'delegatecall', 'staticcall', 'create', 'create2'].includes(op)) effect.barriers.push({ ...origin, reason: 'source assembly interaction/deployment' })
          if (['sstore', 'tstore', 'create', 'create2'].includes(op)) effect.writes.push({ ...origin, reason: 'source assembly storage/deployment effect' })
          return sequence(evaluation, effect)
        }
        default: {
          // Child order is conservative; no display-name protocol assumptions.
          const nodes = Object.values(n).flatMap(v => Array.isArray(v) ? v : [v]).filter(v => v && typeof v === 'object' && v.nodeType)
          return seq(nodes.map(child))
        }
      }
    }
    const effects = visit(decl.body)
    policies.set(decl.id, {
      policy: 'source-order-CEI-compilation-exception-v1',
      allowPostInteractionWrites: effects.witnesses.length > 0,
      claim: 'Preserves pinned source order under the backend conservative helper-call abstraction; NOT a proof of CEI safety.',
      witnesses: effects.witnesses,
    })
  }
  return policies
}
