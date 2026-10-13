const fs = require('fs')
exports.addImports = function (file, names) {
  let s = fs.readFileSync(file, 'utf8')
  const m = s.match(/import \{([^}]*)\} from '\.\/graph-node'/)
  if (!m) throw new Error(`${file}: no named graph-node import`)
  const bare = x => x.trim().replace(/^type /, '')
  const have = new Set(m[1].split(',').map(bare).filter(Boolean))
  const add = names.filter(n => !have.has(bare(n)))
  if (!add.length) return
  const body = m[1]
  const nb = body.includes('\n') ? body.replace(/\s*$/, '').replace(/([^,])$/, '$1,') + `\n    ${add.join(', ')},\n` : body.replace(/\s*$/, '') + `, ${add.join(', ')} `
  s = s.replace(m[0], `import {${nb}} from './graph-node'`)
  fs.writeFileSync(file, s)
}
