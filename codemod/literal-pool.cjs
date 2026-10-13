const path = require('path')
const fs = require('fs')
const ts = require(path.join(__dirname, '../node_modules/typescript'))
const { createProgram } = require('./typed.cjs')
const { addImports } = require('./addimports.cjs')

const root = path.resolve(process.argv.slice(2).find(a => !a.startsWith('--')) ?? path.join(__dirname, '..'))
const dry = process.argv.includes('--dry')
const dir = path.join(root, 'src/engine')
const TARGETS = new Set([
  'getLiteral', 'getOperand', 'getOperands', 'getArgConsts', 'cloneLiterals', 'withRefs', 'withOperand',
  'recordFields', 'setLiteral', 'setOperand', 'encodeSlot', 'encodeOperand', 'makeNode', 'copyNode',
  'createRecordType', 'createFnType', 'createLocatedParam', 'createThisParam',
])

function run(threaded) {
  const { program, checker } = createProgram(root)
  const files = program.getSourceFiles().filter(sf => sf.fileName.startsWith(dir) && !sf.fileName.endsWith('.d.ts') && !sf.fileName.endsWith('graph-node.ts'))
  const typeOf = n => checker.getNonNullableType(checker.getTypeAtLocation(n))
  const spaceLike = n => { const t = typeOf(n); return !!(t.getProperty('nodes') && t.getProperty('ids') && t.getProperty('literals')) }
  const keyOf = d => d && `${path.basename(d.getSourceFile().fileName)}:${d.getStart()}`
  const declOf = e => {
    let s = checker.getSymbolAtLocation(e)
    if (s && s.flags & ts.SymbolFlags.Alias) s = checker.getAliasedSymbol(s)
    const d = s?.valueDeclaration ?? s?.declarations?.[0]
    return d && ts.isVariableDeclaration(d) && d.initializer && (ts.isArrowFunction(d.initializer) || ts.isFunctionExpression(d.initializer)) ? d.initializer : d
  }
  const calleeName = call => ts.isIdentifier(call.expression) ? call.expression : ts.isPropertyAccessExpression(call.expression) ? call.expression.name : undefined
  const isTarget = call => {
    const name = calleeName(call)
    if (!name) return false
    const d = declOf(name)
    if (!d) return false
    if (path.basename(d.getSourceFile().fileName) === 'graph-node.ts') return TARGETS.has(name.text)
    return threaded.has(keyOf(d))
  }
  const names = b => ts.isIdentifier(b) ? [b] : b.elements.flatMap(e => ts.isOmittedExpression(e) ? [] : names(e.name))
  function declared(p, pos) {
    if (ts.isFunctionLike(p)) return (p.parameters ?? []).flatMap(prm => names(prm.name))
    if ((ts.isForStatement(p) || ts.isForOfStatement(p) || ts.isForInStatement(p)) && p.initializer && ts.isVariableDeclarationList(p.initializer)) return p.initializer.declarations.flatMap(d => names(d.name))
    if (ts.isCatchClause(p) && p.variableDeclaration) return names(p.variableDeclaration.name)
    if (ts.isBlock(p) || ts.isSourceFile(p) || ts.isCaseClause(p) || ts.isDefaultClause(p)) {
      return p.statements.filter(st => ts.isVariableStatement(st) && st.getStart() < pos).flatMap(st => st.declarationList.declarations.flatMap(d => names(d.name)))
    }
    return []
  }
  function spaceInScope(call) {
    const shadowed = new Set()
    for (let p = call.parent; p; p = p.parent) {
      const ids = declared(p, call.getStart())
      const hit = ids.find(id => !shadowed.has(id.text) && spaceLike(id))
      if (hit) return hit.text
      for (const id of ids) shadowed.add(id.text)
    }
  }
  function scopeOf(d) {
    let p = d
    while (ts.isBindingElement(p) || ts.isObjectBindingPattern(p) || ts.isArrayBindingPattern(p)) p = p.parent
    if (ts.isParameter(p)) return p.parent
    if (!ts.isVariableDeclaration(p)) return undefined
    const holder = p.parent.parent
    return ts.isVariableStatement(holder) ? holder.parent : holder
  }
  function visibleAt(e, call) {
    let rootId = e
    while (ts.isPropertyAccessExpression(rootId)) rootId = rootId.expression
    if (!ts.isIdentifier(rootId)) return false
    const d = declOf(rootId)
    const scope = d && scopeOf(d)
    if (!scope) return false
    if (call.getStart() < scope.getStart() || call.getEnd() > scope.getEnd()) return false
    if (!ts.isFunctionLike(scope) && d.getStart() > call.getStart()) return false
    return !shadowedBetween(rootId.text, call, scope)
  }
  function shadowedBetween(name, call, scope) {
    for (let p = call.parent; p && p !== scope; p = p.parent) if (declared(p, call.getStart()).some(id => id.text === name)) return true
    return false
  }
  function namedFunction(n) {
    for (let p = n.parent; p; p = p.parent) {
      if (ts.isFunctionDeclaration(p) || ts.isMethodDeclaration(p)) return p
      if ((ts.isArrowFunction(p) || ts.isFunctionExpression(p)) && ts.isVariableDeclaration(p.parent) && ts.isIdentifier(p.parent.name)) return p
    }
  }
  const edits = new Map()
  const edit = (sf, at, end, text) => { const e = edits.get(sf.fileName) ?? []; e.push([at, end, text]); edits.set(sf.fileName, e) }
  const report = { inScope: 0, viaAccess: 0, threaded: 0, values: [] }
  const needs = new Map()
  const params = new Map()
  const labels = new Map()
  for (const sf of files) {
    const accesses = []
    ;(function v(n) {
      if (ts.isPropertyAccessExpression(n) && !(ts.isCallExpression(n.parent) && n.parent.expression === n) && spaceLike(n)) accesses.push(n)
      ts.forEachChild(n, v)
    })(sf)
    ;(function v(n) {
      if (ts.isCallExpression(n) && isTarget(n)) {
        let space = spaceInScope(n)
        if (space) report.inScope++
        else {
          const access = accesses.filter(a => a.getEnd() <= n.getStart() && visibleAt(a, n)).pop()
          if (access) { space = access.getText(sf); report.viaAccess++ }
        }
        if (!space) {
          const f = namedFunction(n)
          if (!f) throw new Error(`${path.basename(sf.fileName)}:${sf.getLineAndCharacterOfPosition(n.getStart()).line + 1}: no space and no named function`)
          needs.set(keyOf(f), f)
          space = '\0' + keyOf(f) + '\0'
          report.threaded++
        }
        const at = n.arguments.pos
        edit(sf, at, at, space + (n.arguments.length ? ', ' : ''))
      } else if (ts.isIdentifier(n) && !(ts.isCallExpression(n.parent) && calleeName(n.parent) === n) && !ts.isImportSpecifier(n.parent) && !(ts.isFunctionDeclaration(n.parent) && n.parent.name === n) && !(ts.isPropertyAccessExpression(n.parent) && n.parent.name === n && ts.isCallExpression(n.parent.parent) && n.parent.parent.expression === n.parent)) {
        const d = declOf(n)
        if (d && d.name !== n && !ts.isPropertyAssignment(n.parent) && ((path.basename(d.getSourceFile().fileName) === 'graph-node.ts' && TARGETS.has(n.text)) || threaded.has(keyOf(d)))) report.values.push(`${path.basename(sf.fileName)}:${sf.getLineAndCharacterOfPosition(n.getStart()).line + 1} ${n.text}`)
      }
      ts.forEachChild(n, v)
    })(sf)
  }
  for (const [key, f] of needs) {
    if (threaded.has(key)) continue
    return { more: [...needs.keys()] }
  }
  for (const [key, f] of [...threaded].map(k => [k, findDecl(program, k)])) {
    const sf = f.getSourceFile()
    const used = new Set()
    ;(function v(n) {
      if (ts.isIdentifier(n) && (n.parent?.name === n || checker.getSymbolAtLocation(n))) used.add(n.text)
      ts.forEachChild(n, v)
    })(f)
    const name = ['g', 'space'].find(x => !used.has(x))
    if (!name) throw new Error(`${key}: both g and space are taken`)
    params.set(key, name)
    labels.set(key, `${path.basename(sf.fileName).replace(/\.ts$/, '.syn')}:${sf.getLineAndCharacterOfPosition(f.getStart()).line + 1} ${f.name?.getText(sf) ?? f.parent.name?.getText(sf)}`)
    const ps = f.parameters
    if (ps.length === 1 && sf.text[ps.pos - 1] !== '(') edit(sf, ps[0].getStart(), ps[0].getEnd(), `(${name}: NodeSpace, ${ps[0].getText(sf)})`)
    else edit(sf, ps.pos, ps.pos, `${name}: NodeSpace${ps.length ? ', ' : ''}`)
  }
  return { edits, params, labels, report }
}

function findDecl(program, key) {
  const [file, start] = key.split(':')
  const sf = program.getSourceFiles().find(s => s.fileName.startsWith(dir) && path.basename(s.fileName) === file)
  let found
  ;(function v(n) {
    if (!found && n.getStart(sf) === +start && ts.isFunctionLike(n)) found = n
    if (!found) ts.forEachChild(n, v)
  })(sf)
  if (!found) throw new Error(`lost ${key}`)
  return found
}

let threaded = new Set()
let result
for (;;) {
  result = run(threaded)
  if (!result.more) break
  threaded = new Set([...threaded, ...result.more])
}
const { edits, params, labels, report } = result
console.log(`in scope ${report.inScope}, via access ${report.viaAccess}, threaded ${report.threaded}`)
for (const [key, name] of params) console.log('threads', name, 'through', labels.get(key))
for (const v of report.values) console.log('value use (fix by hand):', v)
for (const [fileName, list] of edits) {
  const file = fileName.replace(/\.ts$/, '.syn')
  let s = fs.readFileSync(file, 'utf8')
  list.sort((a, b) => b[0] - a[0] || b[1] - a[1])
  for (const [a, b, t] of list) s = s.slice(0, a) + t.replace(/\0([^\0]+)\0/, (m, key) => params.get(key)) + s.slice(b)
  if (dry) continue
  fs.writeFileSync(file, s)
  if (/\bNodeSpace\b/.test(s)) addImports(file, ['type NodeSpace'])
}
