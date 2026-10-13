const ts = require(require('path').join(__dirname, '../node_modules/typescript'))
const fs = require('fs')
const path = require('path')
const { prep } = require('./prep.cjs')
const PRELUDE = `
type u8 = number; type u16 = number; type u32 = number; type u48 = number; type u64 = number; type usize = number
type i8 = number; type i16 = number; type i32 = number; type i64 = number; type f32 = number; type f64 = number
`
exports.createProgram = function (root) {
  const dir = path.resolve(root, 'src/engine')
  const files = fs.readdirSync(dir).filter(f => f.endsWith('.syn')).map(f => path.join(dir, f.replace(/\.syn$/, '.ts')))
  const prelude = path.join(dir, '__prelude.d.ts')
  const cache = new Map()
  const host = ts.createCompilerHost({})
  const read = f => {
    if (f === prelude) return PRELUDE
    if (f.startsWith(dir) && f.endsWith('.ts') && !f.endsWith('.d.ts')) {
      const s = f.replace(/\.ts$/, '.syn')
      if (fs.existsSync(s)) return prep(fs.readFileSync(s, 'utf8'))
    }
    return undefined
  }
  const orig = { getSourceFile: host.getSourceFile, fileExists: host.fileExists, readFile: host.readFile }
  host.getSourceFile = (f, lang) => {
    const t = read(f)
    if (t !== undefined) { if (!cache.has(f)) cache.set(f, ts.createSourceFile(f, t, lang, true, ts.ScriptKind.TS)); return cache.get(f) }
    return orig.getSourceFile(f, lang)
  }
  host.fileExists = f => read(f) !== undefined || orig.fileExists(f)
  host.readFile = f => read(f) ?? orig.readFile(f)
  host.resolveModuleNameLiterals = (lits, containing) => lits.map(l => {
    const n = l.text
    if (n.startsWith('./')) {
      const f = path.join(path.dirname(containing), n.replace(/\.(syn|js|ts)$/, '') + '.ts')
      if (read(f) !== undefined) return { resolvedModule: { resolvedFileName: f, extension: ts.Extension.Ts, isExternalLibraryImport: false } }
    }
    return ts.resolveModuleName(n, containing, {}, host)
  })
  host.resolveModuleNames = (names, containing) => names.map(n => {
    if (n.startsWith('./')) {
      const f = path.join(path.dirname(containing), n.replace(/\.(syn|js|ts)$/, '') + '.ts')
      if (read(f) !== undefined) return { resolvedFileName: f, extension: ts.Extension.Ts, isExternalLibraryImport: false }
    }
    return ts.resolveModuleName(n, containing, {}, host).resolvedModule
  })
  const program = ts.createProgram([prelude, ...files], { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ESNext, moduleResolution: ts.ModuleResolutionKind.Bundler, noEmit: true, allowJs: false, strict: false, skipLibCheck: true, lib: ['lib.es2022.d.ts', 'lib.dom.d.ts'] }, host)
  return { program, checker: program.getTypeChecker(), sourceOf: syn => program.getSourceFile(syn.replace(/\.syn$/, '.ts')) }
}
