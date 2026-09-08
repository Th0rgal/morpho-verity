/** Rendering-only transparent factoring for the generated, first-order CM grammar.
 * Does not change constructor spelling, source order, list shape, options, or opacity.
 * The caller must inventory `definitions` if enabling this optional transformation.
 * Expanding every returned name by its exact `term` recovers the input byte-for-byte.
 * This is not a general Lean parser: only single-line generated source functions
 * and source statements are accepted; comments in these terms fail closed.
 */
const statementConstructors = new Set(`letVar assignVar setStorage setStorageAddr setStorageWord storageArrayPush storageArrayPop setStorageArrayElement setMapping setMappingWord setMappingPackedWord setMapping2 setMapping2Word setMappingUint setMappingChain setStructMember setStructMember2 require requireError revertError return returnValues returnArray returnBytes returnStorageWords returnCodeData mstore tstore calldatacopy returndataCopy revertReturndata stop ite forEach emit internalCall internalCallAssign rawLog externalCallBind tryExternalCallBind ecm unsafeBlock unsafeYul matchAdt`.split(' '));

export function factorGeneratedLeanStatements(source, { prefix = 'renderStatement_', threshold = 0 } = {}) {
  if (!/^[A-Za-z_][A-Za-z0-9_]*_$/.test(prefix)) throw new Error('Invalid factoring prefix');
  if (!Number.isSafeInteger(threshold) || threshold < 0) throw new Error('Invalid factoring threshold');
  if (source.includes(prefix)) throw new Error('Factoring prefix already present');
  const definitions = [];
  const lean = source.split(/(?<=\n)/).map(line => {
    if (!/^def source(?:Function|Statement)_\d+\b/.test(line)) return line;
    const owner = line.match(/^def (\w+)/)[1];
    const tokens = line.match(/"(?:\\.|[^"\\])*"|[\[\](){},]|[^"\[\](){},]+/g) ?? [];
    if (tokens.join('') !== line) throw new Error(`Unsupported string in ${owner}`);
    const stack = [[]];
    const start = definitions.length;
    for (const token of tokens) {
      if (!token.startsWith('"') && /--|\/\-|\-\//.test(token)) throw new Error(`Comments unsupported in ${owner}`);
      if ('[({'.includes(token) && token.length === 1) stack.push([token]);
      else if ('])}'.includes(token) && token.length === 1) {
        if (stack.length === 1) throw new Error(`Unbalanced term in ${owner}`);
        const frame = stack.pop();
        if ('[({'.indexOf(frame[0]) !== '])}'.indexOf(token)) throw new Error(`Mismatched delimiters in ${owner}`);
        if (frame[0] === '[') {
          // Nested delimiters have already been collapsed into single strings.
          const elements = []; let current = '';
          for (const item of frame.slice(1)) {
            if (item === ',') { elements.push(current); current = ''; }
            else current += item;
          }
          elements.push(current);
          for (let i = 0; i < elements.length; i++) {
            const term = elements[i];
            const match = term.match(/^\s*(?:Stmt)?\.([A-Za-z0-9_]+)\b/);
            if (match && statementConstructors.has(match[1]) && term.length >= threshold) {
              const name = `${prefix}${definitions.length}`;
              definitions.push({ name, term, owner, type: 'Stmt' });
              elements[i] = name;
            }
          }
          stack.at(-1).push(`[${elements.join(',')}]`);
        } else stack.at(-1).push(frame.join('') + token);
      } else stack.at(-1).push(token);
    }
    if (stack.length !== 1) throw new Error(`Multiline or unbalanced generated term in ${owner}`);
    return definitions.slice(start).map(({ name, term }) => `def ${name} : Stmt := ${term}\n`).join('') + stack[0].join('');
  }).join('');
  return { lean, definitions };
}
