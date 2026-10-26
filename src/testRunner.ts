import * as api from './api'
import * as fs from 'node:fs'
import * as path from 'node:path'
import * as crypto from 'node:crypto'
import { Worker } from 'node:worker_threads'
import { spawn, spawnSync } from 'node:child_process'
import * as os from 'node:os'
import * as reifiedTypes from './reifiedTypes'
import { createModuleLoader, runSynScript } from './synModules'

const filenameMarker = '// @filename:'

interface VirtualFile {
    name: string
    text: string
    lineoffset?: number
}

function parseTestFile(text: string, extname = '.ts') {
    const compilerOptions: any = {
        declaration: true,
    }

    let line = 0

    let i = 0
    while (i < text.length) {
        const nextMarker = text.indexOf('\n', i)
        if (nextMarker === -1) break

        const l = text.slice(i, nextMarker)
        const m = l.match(/^\/\/ @(\w+): (\w+)/)
        if (!m || m[1] === 'filename') break // compiler options cannot go after filename

        let converted: any = m[2]
        if (converted.startsWith('[') && converted.endsWith(']') || converted === 'true' || converted === 'false') {
            converted = JSON.parse(converted)
        }

        compilerOptions[m[1]] = converted
        i = nextMarker+1
        line += 1
    }

    let currentFile: any = { line, lineoffset: line }
    const files: (VirtualFile & { line: number })[] = []

    while (i < text.length) {
        const nextMarker = text.indexOf(filenameMarker, i)
        if (nextMarker === -1) {
            files.push({
                name: `main${extname}`,
                ...currentFile,
                text: text.slice(i, text.length),
            })
            break
        }

        if (currentFile.name) {
            const t = text.slice(i, nextMarker)
            if (t.trim()) {
                line += t.split('\n').length
                files.push({
                    ...currentFile,
                    text: t,
                })
            }
            currentFile = {}
        }

        const eol = text.indexOf('\n', nextMarker)
        currentFile.name = text.slice(nextMarker + filenameMarker.length, eol).trim()
        currentFile.line = line + 1
        i = eol + 1
    }

    return {
        files,
        compilerOptions,
    }
}

let _vfs: ReturnType<typeof setupVfs> | undefined
function getVfs() {
    return _vfs ??= setupVfs()
}

function setupVfs() {
    const files = new Map<string, string>()

    const _readFile = api.sys.readFile
    const _fileExists = api.sys.fileExists
    const _readDirectory = api.sys.readDirectory

    api.sys.getCurrentDirectory = () => path.resolve('tests')

    function readFile(filePath: string, encoding?: string) {
        const data = files.get(filePath)
        if (data !== undefined) {
            if (encoding === 'bytes') {
                return Buffer.from(data, 'utf-8')
            }
            return data
        }
        return _readFile(filePath, encoding)
    }

    function fileExists(filePath: string) {
        return files.has(filePath) || _fileExists(filePath)
    }

    function readDirectory(...args: Parameters<typeof _readDirectory>) {
        const p = args[0]
        const results = _readDirectory(...args)
        for (const [k, v] of files) {
            if (k.startsWith(p)) {
                results.push(v)
            }
        }
        return results
    }

    Object.assign(api.sys, {
        readFile,
        fileExists,
        readDirectory,
    })

    return {
        files,
    }
}

function createSnapshot(results: VirtualFile[]) {
    let isEmpty = true
    for (const f of results) {
        if (f.text.trim()) {
            isEmpty = false
            break
        }
    }

    if (isEmpty) return ''

    const chunks: string[] = []

    for (const f of results) {
        chunks.push(`${filenameMarker} ${f.name}`)
        chunks.push(f.text)
        chunks.push('')
    }

    return chunks.join('\n')
}

async function writeFile(p: string, d: string | Buffer) {
    await fs.promises.mkdir(path.dirname(p), { recursive: true })
    await fs.promises.writeFile(p, d)
}

async function runTestCase(name: string, opt?: { testOnly?: boolean; shouldExecute?: boolean }) {
    const testsDir = path.resolve('tests')
    const casesDir = path.resolve(testsDir, 'cases')
    const snapshotDir = path.resolve(testsDir, 'snapshots')

    const absPath = path.resolve(casesDir, name)
    const relPath = path.relative(casesDir, absPath)

    const text = await fs.promises.readFile(absPath, 'utf-8')
    const parsed = parseTestFile(text, path.extname(absPath))
    const vfs = getVfs()
    vfs.files.clear()

    const resolvedMap = new Map<string, VirtualFile & { line: number }>()

    const roots: string[] = []
    for (const item of parsed.files) {
        const resolved = path.resolve(testsDir, item.name)
        resolvedMap.set(resolved, item)

        if (item.name.endsWith('.json')) {
            const txt = item.text.at(0) === '(' && item.text.at(-1) === ')'
                ? item.text.slice(1, -1) // removes ()
                : item.text
            vfs.files.set(resolved, txt) 
            continue
        }

        if (item.name.startsWith('node_modules')) {
            vfs.files.set(resolved, item.text)
            continue
        }

        roots.push(resolved)
        vfs.files.set(resolved, item.text)
    }

    const prog = api.createProgram(roots, parsed.compilerOptions)
    const results: (VirtualFile & { source: string })[] = []
    const promises: Promise<void>[] = []

    const sourceFiles = roots.map(f => {
        const sf = prog.getSourceFileByPath(f as any)
        if (!sf) {
            throw new Error(`missing source file: ${f}`)
        }
        return sf
    })

    for (const sf of sourceFiles) {
        const p = new Promise<void>((resolve, reject) => {
            const source = sf.fileName
            const expected = new Set<string>()
            const x = prog.emit(sf, (name, text) => {
                results.push({ name, text, source })
                expected.delete(name)
                if (expected.size === 0) {
                    resolve()
                }
            })
            x.emittedFiles?.forEach(f => expected.add(f))
        })
        promises.push(p)
    }

    await Promise.all(promises)

    const allErrors: string[] = []
    for (const sf of sourceFiles) {
        const r = path.relative(process.cwd(), absPath)
        const item = resolvedMap.get(sf.fileName)!
        const formatted = (prog.getSemanticDiagnostics(sf) as any as string).trim()
        const replaced = formatted.replaceAll(/([^\s:]+):(\d+):(\d+)/g, (_, n, l, c) => {
            return `${r}:${Number(l)+item.line}:${c}`
        })
        if (replaced) {
            allErrors.push(replaced)
        }
    }

    {
        const snapshotPath = path.resolve(snapshotDir, relPath.replace(/\.(?:syn|tsx?)$/, '.errors'))
        const existingSnapshot = await fs.promises.readFile(snapshotPath, 'utf-8').catch(err => {
            if ((err as any).code !== 'ENOENT') {
                throw err
            }
        })

        if (allErrors.length) {
            const d = allErrors.join('\n')
            console.log('----- diagnostics -----')
            console.log(d)
            await writeFile(snapshotPath, d)
        } else {
            console.log('----- no diagnostics -----')
            await fs.promises.rm(snapshotPath).catch(err => {
                if ((err as any).code !== 'ENOENT') {
                    throw err
                }
            })
        }
    }

    const groups = new Map<string, (VirtualFile & { order: number; absPath: string })[]>()
    for (const f of results) {
        const groupName = path.basename(f.name).match(/\.(.*)$/)?.[0]
        if (!groupName) {
            throw new Error(`failed parse file type: ${f.name}`)
        }
        let group = groups.get(groupName)
        if (!group) {
            groups.set(groupName, group = [])
        }

        const name = path.relative(testsDir, f.source)
        const order = roots.indexOf(f.source)
        group.push({ 
            name, 
            text: f.text, 
            order,
            absPath: f.source,
            lineoffset: parsed.files[order].lineoffset,
        })
    }

    for (const [k, v] of groups) {
        v.sort((a, b) => a.order - b.order)

        const newSnapshot = createSnapshot(v)
        const snapshotPath = path.resolve(snapshotDir, relPath.replace(/\.(?:syn|tsx?)$/, k))
        const existingSnapshot = await fs.promises.readFile(snapshotPath, 'utf-8').catch(err => {
            if ((err as any).code !== 'ENOENT') {
                throw err
            }
        })

        if ((opt?.shouldExecute || parsed.compilerOptions.run) && k === '.js') {
            const f = v[0]
            const reifier = (prog as any).getReifier()
            runSynScript(f.text, f.absPath, reifier)
        }

        if (existingSnapshot === undefined) {
            if (newSnapshot) {
                await writeFile(snapshotPath, newSnapshot)
            }
            continue
        }

        const diff = lineDiff(existingSnapshot, newSnapshot)
        if (diff.length === 0) {
            continue
        }

        if (!opt?.testOnly) {
            console.log(`snapshot changed: ${path.relative(process.cwd(), snapshotPath)}`)
            if (!newSnapshot) {
                await fs.promises.rm(snapshotPath).catch(err => {
                    if ((err as any).code !== 'ENOENT') {
                        throw err
                    }
                })
            } else {
                await writeFile(snapshotPath, newSnapshot)
            } 
        } else {
            console.log(diff)
            throw new Error(`snapshot did not match: ${path.relative(process.cwd(), snapshotPath)}`)
        }
    }
}

async function runVsonTestCase(name: string) {
    const testsDir = path.resolve('tests')
    const casesDir = path.resolve(testsDir, 'cases')
    const snapshotDir = path.resolve(testsDir, 'snapshots')

    const absPath = path.resolve(casesDir, name)
    const relPath = path.relative(casesDir, absPath)

    const text = await fs.promises.readFile(absPath, 'utf-8')
    const parsed = parseTestFile(text, '.vson')
    if (parsed.files.length !== 1) {
        throw new Error(`.vson test cases only support 1 file, got ${parsed.files.length} (${name})`)
    }

    const emitVson = parsed.compilerOptions.emit === 'vson'

    let output: string
    try {
        output = api.optimizeVson(parsed.files[0].text, emitVson)
    } catch (e: any) {
        output = `error: ${e?.message ?? e}`
    }

    const snapshotExt = emitVson ? '.vson' : '.js'
    const snapshotPath = path.resolve(snapshotDir, relPath.replace(/\.vson$/, snapshotExt))
    const existingSnapshot = await fs.promises.readFile(snapshotPath, 'utf-8').catch((err: any) => {
        if (err.code !== 'ENOENT') {
            throw err
        }
    })

    if (existingSnapshot === undefined) {
        await writeFile(snapshotPath, output)
        return
    }

    if (existingSnapshot === output) {
        return
    }

    console.log(`snapshot changed: ${path.relative(process.cwd(), snapshotPath)}`)
    await writeFile(snapshotPath, output)
}

const engineCompilerOptions = {
    lib: ['reify', 'machine'],
    sourceMap: true,
    inlineSourceMap: true,
}

interface EngineCacheEntry {
    inputs: Record<string, string>
    emitted: Record<string, string>
}

function contentHash(data: string | Buffer) {
    return crypto.createHash('sha256').update(data).digest('hex')
}

function readEngineCache(file: string): Map<string, string> | undefined {
    let entry: EngineCacheEntry
    try {
        entry = JSON.parse(fs.readFileSync(file, 'utf-8'))
    } catch {
        return
    }
    for (const [name, hash] of Object.entries(entry.inputs)) {
        let data: Buffer
        try {
            data = fs.readFileSync(name)
        } catch {
            return
        }
        if (contentHash(data) !== hash) return
    }
    return new Map(Object.entries(entry.emitted))
}

function writeEngineCache(file: string, entry: EngineCacheEntry) {
    const dir = path.dirname(file)
    fs.mkdirSync(dir, { recursive: true })
    const tmp = `${file}.${process.pid}.tmp`
    fs.writeFileSync(tmp, JSON.stringify(entry))
    fs.renameSync(tmp, file)
    const stale = fs.readdirSync(dir)
        .filter(name => name.endsWith('.json'))
        .map(name => path.resolve(dir, name))
        .sort((a, b) => fs.statSync(b).mtimeMs - fs.statSync(a).mtimeMs)
        .slice(8)
    for (const name of stale) fs.rmSync(name, { force: true })
}

async function emitEngine(roots: string[]): Promise<EngineCacheEntry> {
    const prog = await api.createProgramAsync(roots, engineCompilerOptions)

    const emitted: Record<string, string> = {}
    const sourceFiles = prog.getSourceFiles().filter(sf => sf.fileName.endsWith('.syn'))
    const promises = []
    for (const sf of sourceFiles) {
        const p = new Promise<void>((resolve, reject) => {
            const expected = new Set<string>()
            const x = prog.emit(sf, (name, text) => {
                emitted[name] = text
                expected.delete(name)
                if (expected.size === 0) {
                    resolve()
                }
            })
            x.emittedFiles?.forEach(f => expected.add(f))
        })
        promises.push(p)
    }

    await Promise.all(promises)

    const inputs: Record<string, string> = {}
    for (const sf of prog.getSourceFiles()) {
        if (fs.existsSync(sf.fileName)) inputs[sf.fileName] = contentHash(fs.readFileSync(sf.fileName))
    }
    return { inputs, emitted }
}

let launcher = [process.argv[1]]

function hostCommand(command: string, args: string[]): [string, string[]] {
    if (command !== process.argv[1]) return [command, args]
    return [launcher[0], [...launcher.slice(1), ...args]]
}

async function compileEngine(argv: string[]) {
    const files = await fs.promises.readdir(path.resolve('src/engine'), { recursive: true, withFileTypes: true })

    const roots = files.filter(x => x.isFile() && x.name.endsWith('.syn')).map(x => path.resolve(x.parentPath, x.name)).sort()
    const sources = roots.map(r => [r, contentHash(fs.readFileSync(r))])
    const key = contentHash(JSON.stringify([api.getBuildId(), contentHash(fs.readFileSync(path.resolve('src/api.ts'))), engineCompilerOptions, sources]))
    const cacheFile = path.resolve('out/engine-cache', `${key}.json`)
    let emitted = readEngineCache(cacheFile)
    if (!emitted) {
        const entry = await emitEngine(roots)
        writeEngineCache(cacheFile, entry)
        emitted = new Map(Object.entries(entry.emitted))
    }

    const reifier = {
        types: reifiedTypes.createTypeNamespace(),
        __reify: undefined,
        __readFile: (file: string) => fs.readFileSync(file, 'utf8'),
        __writeFile: (file: string | number, text: string | Uint8Array) => {
            if (typeof file === 'string') fs.mkdirSync(path.dirname(file), { recursive: true })
            fs.writeFileSync(file, text)
        },
        __readBytes: (file: string) => new Uint8Array(fs.readFileSync(file)),
        __readDir: (dir: string) => fs.readdirSync(dir),
        __spawnSync: (command: string, args: string[], env: Record<string, string>) => {
            const exited = spawnSync(...hostCommand(command, args), { env, encoding: 'utf8' })
            return { status: exited.status, signal: exited.signal, stdout: exited.stdout, stderr: exited.stderr }
        },
    }
    const loader = createModuleLoader(reifier, (from, name) => {
        let r = path.resolve(path.dirname(from), name)
        if (!path.extname(r)) r = `${r}.js`
        return [r, emitted.get(r)]
    }, api, argv.some(a => a.startsWith('coverage:')) ? undefined : path.resolve('out/engine-cache/v8'))
    return { emitted, loader }
}

let engine: ReturnType<typeof compileEngine> | undefined

async function testEngine(suite = 'heap', argv: string[] = []): Promise<number> {
    const { emitted, loader } = await (engine ??= compileEngine(argv))
    const harness = path.resolve('src/engine/harness.js')
    const prof = argv.find(a => a.startsWith('cpu-prof:') || a.startsWith('heap-prof:') || a.startsWith('coverage:'))?.replace(/^(cpu-prof|heap-prof|coverage):/, '')
    if (prof) {
        fs.mkdirSync(`${prof}.src`, { recursive: true })
        for (const [name, text] of emitted) fs.writeFileSync(path.join(`${prof}.src`, path.basename(name)), text)
    }
    const engineArgv = argv.slice(1).filter(a => !/^(baseline|max-crashes):/.test(a))
    return loader.run(harness, emitted.get(harness)!, engineArgv, (exports: any) => exports.runSuite(suite)) as Promise<number>
}

function lineDiff(a: string, b: string) {
    const left = a.split('\n')
    const right = b.split('\n')
    const diffs = []
    let i = 0
    let j = 0

    while (i < left.length || j < right.length) {
        const l = left[i++]
        const r = right[j++]
        if (l === r) {
            continue
        }

        if (r !== undefined && !left.includes(r)) {
            diffs.push({ type: 'added', line: r, index: j - 1 })
            i--
        } else if (l !== undefined && !right.includes(l)) {
            diffs.push({ type: 'removed', line: l, index: i - 1 })
            j--
        }
    }

    return diffs
}

async function gatherTestCases() {
    const casesDir = path.resolve('tests', 'cases')
    const files = await fs.promises.readdir(casesDir, { recursive: true, withFileTypes: true })

    return files.filter(x => x.isFile()).map(x => path.resolve(x.parentPath, x.name))
}

async function profiled<T>(file: string | undefined, heapFile: string | undefined, run: () => Promise<T>, coverageFile?: string): Promise<T> {
    if (!file && !heapFile && !coverageFile) return run()
    const inspector = await import('node:inspector')
    const session = new inspector.Session()
    session.connect()
    const post = (method: string, params?: object) => new Promise<any>((resolve, reject) => {
        session.post(method, params ?? {}, (err: any, res: any) => err ? reject(err) : resolve(res))
    })
    if (file) {
        await post('Profiler.enable')
        await post('Profiler.setSamplingInterval', { interval: 100 })
        await post('Profiler.start')
    }
    if (coverageFile) {
        await post('Profiler.enable')
        await post('Profiler.startPreciseCoverage', { callCount: true, detailed: true })
    }
    if (heapFile) {
        await post('HeapProfiler.enable')
        await post('HeapProfiler.startSampling', {
            samplingInterval: 4096,
            includeObjectsCollectedByMajorGC: true,
            includeObjectsCollectedByMinorGC: true,
        })
    }
    try {
        return await run()
    } finally {
        if (file) {
            const { profile } = await post('Profiler.stop')
            fs.writeFileSync(file, JSON.stringify(profile))
        }
        if (coverageFile) {
            const { result } = await post('Profiler.takePreciseCoverage')
            fs.writeFileSync(coverageFile, JSON.stringify(result.filter((script: any) => script.url.includes('/src/engine/'))))
        }
        if (heapFile) {
            const { profile } = await post('HeapProfiler.stopSampling')
            fs.writeFileSync(heapFile, JSON.stringify(profile))
        }
    }
}

declare var process: any

const watchdogSource = `
const { workerData } = require('node:worker_threads')
const fs = require('node:fs')
const wait = new Int32Array(new SharedArrayBuffer(4))
while (Date.now() < workerData.deadline) {
    Atomics.wait(wait, 0, 0, Math.min(250, workerData.deadline - Date.now()))
    if (workerData.ppid && process.ppid !== workerData.ppid) process.kill(workerData.pid, 'SIGKILL')
}
fs.writeSync(2, '\\nwatchdog: killed after ' + workerData.seconds + 's\\n')
process.kill(workerData.pid, 'SIGKILL')
`

function startWatchdog(seconds: number, deadline = Date.now() + seconds * 1000, ppid = 0): void {
    if (!(seconds > 0) && !ppid) return
    const workerData = { seconds, deadline: seconds > 0 ? deadline : Infinity, pid: process.pid, ppid }
    const worker = new Worker(watchdogSource, { eval: true, workerData })
    worker.unref()
}

const allTestableFiles = ['heap', 'bytecode-emitter', 'trace2', 'cg', 'containers', 'vm', 'maps', 'closures', 'mini-optimizer', 'lowering-repros', 'native', 'profile', 'sampling']

interface CrashResume {
    suite: string
    case: number
}

const crashFd = Number(process.env.SYN_CRASH_FD ?? 0)
const crashResume: CrashResume | undefined = process.env.SYN_CRASH_RESUME ? JSON.parse(process.env.SYN_CRASH_RESUME) : undefined
const crashSignals = new Set(['SIGSEGV', 'SIGBUS', 'SIGTRAP', 'SIGILL', 'SIGABRT', 'SIGFPE', 'SIGSYS'])

interface CrashMarks {
    suite?: string
    caseNumber?: number
    caseLabel?: string
    caseName?: string
    step?: string
    eventLabel?: string
}

function readMark(marks: CrashMarks, line: string): void {
    const fields = line.split('\t')
    if (fields[0] === 'suite') {
        marks.suite = fields[1]
        marks.caseNumber = undefined
        marks.step = undefined
        marks.eventLabel = undefined
    } else if (fields[0] === 'case') {
        marks.caseNumber = Number(fields[1])
        marks.caseLabel = fields[2]
        marks.caseName = fields[3]
        marks.step = undefined
        marks.eventLabel = undefined
    } else if (fields[0] === 'step') {
        marks.step = fields[1]
    } else if (fields[0] === 'event') {
        marks.eventLabel = fields[1]
    } else if (fields[0] === 'end') {
        marks.caseNumber = undefined
        marks.step = undefined
        marks.eventLabel = undefined
    }
}

interface BaselineSection {
    problems: string[]
    cases: string[]
    ms: number
    phases: Record<string, number>
    done: boolean
    since?: number
    fingerprint?: string
}

function caseMs(ms: number): number {
    if (ms < 0.5) return 0
    const scale = 10 ** Math.floor(Math.log10(Math.max(1, ms)))
    const mantissa = ms / scale
    if (mantissa < 1.4) return scale
    if (mantissa < 3.2) return 2 * scale
    if (mantissa < 7.1) return 5 * scale
    return 10 * scale
}

function stopClock(section: BaselineSection | undefined): void {
    if (section?.since === undefined) return
    section.ms += performance.now() - section.since
    section.since = undefined
}

function baselineFile(): string {
    const model = os.cpus()[0]?.model ?? 'unknown'
    return path.resolve('baselines', `${model.toLowerCase().replace(/[^a-z0-9]+/g, '-')}-${os.arch()}.txt`)
}

function baselineSections(file: string): Map<string, string[]> {
    const sections = new Map<string, string[]>()
    const text = fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : ''
    let body: string[] = []
    for (const line of text.split('\n')) {
        const fields = line.split(' ')
        if (line.startsWith('== ')) {
            body = []
            sections.set(line.slice(3), body)
        } else if (fields[0] === 'CASE') {
            const at = fields.length - (fields.length > 5 && /^\d+$/.test(fields[fields.length - 3]) ? 3 : 2)
            const key = `CASE ${fields[1]}`
            sections.set(key, [...sections.get(key) ?? [], `case ${fields.slice(2, at).join(' ')} ${fields[at]} ${fields[at + 1]} 0 0`])
        } else if (line) {
            body.push(line)
        }
    }
    return sections
}

function sectionBody(section: BaselineSection, load: number): string[] {
    const accounted = Object.values(section.phases).reduce((a, b) => a + b, 0)
    const phases = Object.entries({ ...section.phases, setup: Math.max(0, section.ms - accounted) })
    const percents = phases.map(([phase, ms]) => [phase, Math.round(ms * 100 / Math.max(1, section.ms))] as const).filter(x => x[1] > 0)
    const fingerprint = section.fingerprint ? [`fingerprint ${section.fingerprint}`] : []
    return [`total ${(section.ms / 1000).toFixed(1)}s load ${load}`, ...fingerprint, `phases ${percents.map(([phase, pct]) => `${phase} ${pct}%`).join(' ')}`, ...section.problems, ...section.cases]
}

function caseResults(body: string[]) {
    const cases = new Map<string, [number, number]>()
    const broken = new Map<string, string>()
    const phases = new Map<string, number>()
    let seconds = 0
    let fingerprint: { compiler: string, roots: string } | undefined
    for (const line of body) {
        const fields = line.split(' ')
        if (fields[0] === 'total') seconds = parseFloat(fields[1])
        if (fields[0] === 'fingerprint') fingerprint = { compiler: fields[2], roots: fields[4] }
        if (fields[0] === 'phases') {
            for (let i = 1; i + 1 < fields.length; i += 2) phases.set(fields[i], parseFloat(fields[i + 1]))
        }
        if (fields[0] === 'case') cases.set(fields.slice(1, -4).join(' '), [Number(fields[fields.length - 4]), Number(fields[fields.length - 3])])
        if (/^(fail|crash|cut) /.test(line)) broken.set(line.slice(fields[0].length + 1, line.indexOf(': ')), line)
    }
    return { cases, broken, phases, seconds, fingerprint }
}

function fingerprintDelta(before: ReturnType<typeof caseResults>, after: ReturnType<typeof caseResults>, changed: number): string {
    if (!after.fingerprint) return ''
    if (!before.fingerprint) return ', no baseline fingerprint'
    const inputs: string[] = []
    if (before.fingerprint.compiler !== after.fingerprint.compiler) inputs.push('compiler changed')
    if (before.fingerprint.roots !== after.fingerprint.roots) inputs.push('suite inputs changed')
    if (inputs.length) return `, ${inputs.join(', ')}`
    return changed ? ', same compiler and inputs: nondeterminism or environment' : ', same compiler and inputs'
}

function signed(x: number): string {
    return x > 0 ? `+${x}` : `${x}`
}

function reportDeltas(key: string, old: string[] | undefined, body: string[], partial: boolean): number {
    if (!old) {
        console.log(`DELTAS ${key}: no baseline section`)
        return 0
    }
    const before = caseResults(old)
    const after = caseResults(body)
    const lines: string[] = []
    const totals = [0, 0, 0, 0]
    let changed = 0
    let removed = 0
    for (const [name, [insts, nonMov]] of after.cases) {
        const was = before.cases.get(name)
        if (after.broken.has(name) || before.broken.has(name)) continue
        if (!was) {
            lines.push(`new ${name} ${insts}/${nonMov}`)
            continue
        }
        totals[0] += was[0]
        totals[1] += was[1]
        totals[2] += insts
        totals[3] += nonMov
        if (was[0] === insts && was[1] === nonMov) continue
        changed++
        lines.push(`${name} ${was[0]}/${was[1]} -> ${insts}/${nonMov} (${signed(insts - was[0])})`)
    }
    for (const [name, [insts, nonMov]] of before.cases) {
        if (partial || after.cases.has(name) || after.broken.has(name)) continue
        lines.push(`removed ${name} (was ${insts}/${nonMov})`)
        removed++
    }
    for (const [name, line] of after.broken) {
        if (!before.broken.has(name)) lines.push(`new failure ${line}`)
    }
    for (const [phase, pct] of after.phases) {
        const was = before.phases.get(phase) ?? 0
        if (!partial && before.seconds >= 5 && after.seconds >= 5 && pct >= 1.5 * was && pct - was >= 3) lines.push(`phase ${phase} ${was}% -> ${pct}%`)
    }
    for (const name of before.broken.keys()) {
        if (!after.broken.has(name) && after.cases.has(name)) lines.push(`fixed ${name}`)
    }
    console.log(`DELTAS ${key}: ${changed} changed, ${totals[0]}/${totals[1]} -> ${totals[2]}/${totals[3]} (${signed(totals[2] - totals[0])}/${signed(totals[3] - totals[1])})${fingerprintDelta(before, after, changed)}${partial ? ', partial run' : ''}`)
    for (const line of lines) console.log(`DELTA ${key}: ${line}`)
    if (removed) console.log(`${key} FAILED: ${removed} of ${before.cases.size} baseline cases did not run (${after.cases.size} ran); a suite that drops cases is a failure`)
    return removed
}

function writeBaselines(bodies: Map<string, string[]>): void {
    const file = baselineFile()
    const kept = baselineSections(file)
    for (const [key, body] of bodies) kept.set(key, body)
    const cpus = os.cpus()
    const header = [`os ${spawnSync('uname', ['-sr'], { encoding: 'utf8' }).stdout.trim()}`, `cpu ${cpus[0]?.model ?? 'unknown'}`, `arch ${os.arch()}`, `cores ${cpus.length}`, 'format: case <name> <insts> <non-mov> <fns> <ms rounded to 1-2-5 steps>; fail|crash|cut <case>: <detail>; fingerprint compiler <createNatives hash> roots <hash of the roots the suite passes to createNatives>']
    const sections = [...kept.keys()].sort().map(key => [`== ${key}`, ...kept.get(key)!].join('\n') + '\n')
    fs.mkdirSync(path.dirname(file), { recursive: true })
    fs.writeFileSync(file, [header.join('\n') + '\n', ...sections].join('\n'))
}

function crashOf(code: number | null, signal: string | null): string | undefined {
    const signals = os.constants.signals as Record<string, number>
    const name = signal ?? Object.keys(signals).find(k => code !== null && code > 128 && signals[k] === code - 128)
    if (!name || !crashSignals.has(name)) return
    return `CRASH signal ${signals[name]} (exit ${128 + signals[name]})`
}

interface CodeUnit {
    entry: number
    size: number
    bytes: number
    link: number
    name: string
}

function symbolizeCrash(maps: string[], dump: Buffer): string[] {
    if (dump.length < 320) return [dump.length ? `  crash dump truncated at ${dump.length} bytes` : '  no crash dump']
    const units: CodeUnit[] = []
    for (const line of maps) {
        for (const field of line.split('\t').slice(1)) {
            const [entry, size, bytes, link, ...name] = field.split(' ')
            units.push({ entry: parseInt(entry, 16), size: Number(size), bytes: Number(bytes), link: Number(link), name: name.join(' ') })
        }
    }
    units.sort((a, b) => a.entry - b.entry)
    const unitAt = (address: number): CodeUnit | undefined => {
        let lo = 0
        let hi = units.length
        while (lo < hi) {
            const mid = (lo + hi) >> 1
            if (units[mid].entry <= address) lo = mid + 1
            else hi = mid
        }
        const unit = units[lo - 1]
        return unit && address < unit.entry + unit.size ? unit : undefined
    }
    const callerAt = (address: number): CodeUnit | undefined => {
        const unit = unitAt(address)
        return unit && address > unit.entry ? unit : undefined
    }
    const hex = (address: number) => `0x${address.toString(16)}`
    const symbol = (address: number): string => {
        const unit = unitAt(address)
        return unit ? `${unit.name}+${hex(address - unit.entry)}` : `${hex(address)} (outside native code)`
    }
    const word = (at: number) => Number(dump.readBigUInt64LE(at))
    const signo = dump.readInt32LE(0)
    const lr = word(288)
    const top = word(296)
    const pc = word(304)
    const stackWord = (address: number): number | undefined => {
        const at = 320 + address - top
        return address >= top && at + 8 <= dump.length ? word(at) : undefined
    }
    const signal = Object.entries(os.constants.signals).find(([, n]) => n === signo)?.[0] ?? `signal ${signo}`
    const lines = [`  ${signal} at ${symbol(pc)}, fault address ${hex(word(24))}, sp ${hex(top)}`]
    const frames = [symbol(pc)]
    const push = (address: number, how: string) => frames.push(`${symbol(address)}${how}`)
    let unit = unitAt(pc)
    let sp: number | undefined = top
    let lower = top
    while (frames.length < 256) {
        const first = frames.length === 1
        if (unit && sp !== undefined && unit.bytes >= 0) {
            const entering = first && pc === unit.entry
            const ret = unit.link >= 0 && !entering ? stackWord(sp + unit.link) : first ? lr : undefined
            if (ret === undefined) break
            if (callerAt(ret)) {
                push(ret, '')
                sp += entering ? 0 : unit.bytes
                lower = sp
                unit = callerAt(ret)
                continue
            }
            if (!first) {
                push(ret, '')
                break
            }
        }
        if (first && callerAt(lr)) {
            push(lr, ' (lr)')
            unit = callerAt(lr)
            sp = undefined
            continue
        }
        const known = unit && unit.link >= 0 && unit.bytes >= 0 ? unit : undefined
        let at = lower + (known ? known.link : 0)
        while (stackWord(at) !== undefined && !callerAt(stackWord(at)!)) at += 8
        const ret = stackWord(at)
        if (ret === undefined) break
        push(ret, ' (scan)')
        sp = known ? at - known.link + known.bytes : undefined
        lower = sp ?? at + 8
        unit = callerAt(ret)
    }
    lines.push(`  backtrace via frame layouts (lr = link register, scan = stack scan, may be stale):`)
    let i = 0
    while (i < frames.length) {
        let j = i
        while (j + 1 < frames.length && frames[j + 1] === frames[i]) j++
        lines.push(`    #${i} ${frames[i]}${j > i ? ` (repeated ${j - i + 1} times)` : ''}`)
        i = j + 1
    }
    if (frames.length >= 256) lines.push('    ... truncated')
    return lines
}

async function superviseEngine(args: string[], seconds: number): Promise<void> {
    const suites = args[1] === 'all' ? allTestableFiles : [args[1]]
    const deadline = Date.now() + seconds * 1000
    const crashes: string[] = []
    const given = args.find(a => a.startsWith('max-crashes:'))
    const maxCrashes = given ? Number(given.slice('max-crashes:'.length)) : 3
    let resume: CrashResume | undefined
    let exitCode = 0
    let failures = 0
    let killed: string | undefined
    const load = Math.round(os.loadavg()[0])
    const compared = args.find(a => a.startsWith('baseline:'))?.slice('baseline:'.length) ?? baselineFile()
    const baseline = baselineSections(compared)
    const sections = new Map<string, BaselineSection>()
    let section: BaselineSection | undefined
    while (true) {
        const env: Record<string, string> = { ...process.env, SYN_CRASH_FD: '3', SYN_CRASH_DUMP_FD: '4', SYN_CRASH_DEADLINE: String(seconds > 0 ? deadline : 0), SYN_CRASH_PPID: String(process.pid) }
        if (resume) env.SYN_CRASH_RESUME = JSON.stringify(resume)
        const child = spawn(...hostCommand(process.argv[1], process.argv.slice(2)), { stdio: ['inherit', 'inherit', 'inherit', 'pipe', 'pipe'], env })
        const marks: CrashMarks = {}
        const maps: string[] = []
        const dump: Buffer[] = []
        let pending = ''
        child.stdio[4]!.on('data', (chunk: Buffer) => dump.push(chunk))
        child.stdio[3]!.setEncoding('utf8')
        child.stdio[3]!.on('data', (chunk: string) => {
            const lines = (pending + chunk).split('\n')
            pending = lines.pop()!
            for (const line of lines) {
                const fields = line.split('\t')
                if (fields[0] === 'fail') {
                    failures++
                    section?.problems.push(`fail ${marks.caseNumber !== undefined ? marks.caseName : '-'}: ${line.slice(5)}`)
                    continue
                }
                if (fields[0] === 'map') {
                    maps.push(line)
                    continue
                }
                readMark(marks, line)
                if (fields[0] === 'suite') {
                    stopClock(section)
                    section = sections.get(fields[2]) ?? { problems: [], cases: [], ms: 0, phases: {}, done: false }
                    sections.set(fields[2], section)
                    section.since = performance.now()
                } else if (fields[0] === 'end' && fields.length > 2 && section) {
                    section.cases.push(`case ${fields[1]} ${fields[2]} ${fields[3]} ${fields[4]} ${caseMs(Number(fields[5]))}`)
                } else if (fields[0] === 'fingerprint' && section) {
                    section.fingerprint = `compiler ${fields[1]} roots ${fields[2]}`
                } else if (fields[0] === 'done' && section) {
                    stopClock(section)
                    section.done = true
                    for (const [phase, ms] of Object.entries(JSON.parse(fields[1]) as Record<string, number>)) section.phases[phase] = (section.phases[phase] ?? 0) + ms
                }
            }
        })
        const [code, signal] = await new Promise<[number | null, string | null]>(resolve => child.on('close', (c, s) => resolve([c, s])))
        const message = crashOf(code, signal)
        stopClock(section)
        const running = marks.caseNumber !== undefined ? marks.caseName : '-'
        if (!message) {
            exitCode = code ?? 128 + ((os.constants.signals as Record<string, number>)[signal!] ?? 0)
            if (signal) killed = seconds > 0 && Date.now() >= deadline ? `watchdog timeout after ${seconds}s` : `killed by ${signal}`
            if (killed) section?.problems.push(`cut ${running}: ${killed}`)
            break
        }
        let label = marks.suite ?? args[1]
        let next: CrashResume | undefined
        if (marks.suite !== undefined && marks.caseNumber !== undefined) {
            label = marks.step ?? (marks.eventLabel ? `${marks.suite} ${marks.eventLabel}` : marks.caseLabel!)
            next = { suite: marks.suite, case: marks.caseNumber }
        } else {
            if (marks.eventLabel) label = `${label} in ${marks.eventLabel}`
            const following = suites[suites.indexOf(marks.suite!) + 1]
            if (marks.suite !== undefined && following) next = { suite: following, case: 0 }
        }
        console.log(`${label} FAILED: ${message}`)
        if (marks.caseNumber !== undefined) console.log(`  case ${marks.caseLabel}`)
        for (const line of symbolizeCrash(maps, Buffer.concat(dump))) console.log(line)
        section?.problems.push(`crash ${running}: ${label} ${message}`)
        crashes.push(label)
        if (!next || crashes.length >= maxCrashes) {
            console.log('...max crash count exceeded')
            section?.problems.push(`cut ${running}: max crash count exceeded`)
            exitCode = 128 + Number(message.match(/signal (\d+)/)![1])
            break
        }
        resume = next
    }
    const filtered = args.some(a => /^(only|skip|bench|prof|sample)(:|$)/.test(a))
    const bodies = new Map([...sections].map(([key, section]) => [key, sectionBody(section, load)]))
    for (const [key, body] of [...bodies]) {
        if (!reportDeltas(key, baseline.get(key) ?? baseline.get(`CASE ${key.split(' ')[0]}`), body, filtered || !sections.get(key)!.done)) continue
        failures++
        bodies.delete(key)
    }
    if (bodies.size && !filtered) writeBaselines(bodies)
    if (crashes.length) console.log(`CRASHES ${crashes.length}: ${crashes.join('; ')}`)
    if (!exitCode && (failures || crashes.length)) exitCode = 1
    const problems = [`${failures} failures`, `${crashes.length} crashes`]
    if (killed) problems.push(killed)
    console.log(`RESULT ${exitCode ? 'FAILED' : 'ok'}: ${problems.join(', ')}, exit ${exitCode}`)
    if (args[1] === 'all') console.log(`TOTAL TIME ${process.uptime().toFixed(3)}s`)
    process.exitCode = exitCode
}

function report(failures: number): void {
    if (failures) process.exitCode = 1
    if (!crashFd) console.log(`RESULT ${failures ? 'FAILED' : 'ok'}: ${failures} failures, exit ${failures ? 1 : 0}`)
}

export async function main(...args: string[]) {
    const files = await gatherTestCases()
    const filter = args[0]
    if (filter === '--engine') {
        const given = args.find(a => a.startsWith('timeout:'))
        const seconds = given ? Number(given.slice('timeout:'.length)) : args[1] === 'all' ? 90 : 45
        if (seconds > 180) throw '3 minutes is all you need :)'
        const profiling = args[1] !== 'all' && args.some(a => /^(cpu-prof|heap-prof|coverage):/.test(a))
        if (!crashFd && !profiling && !args.includes('in-process')) return superviseEngine(args, seconds)
        if (crashFd) startWatchdog(seconds, Number(process.env.SYN_CRASH_DEADLINE), Number(process.env.SYN_CRASH_PPID))
        else startWatchdog(seconds)
    }
    if (filter === '--engine' && args[1] !== 'all') {
        const prof = args.find(a => a.startsWith('cpu-prof:'))?.slice('cpu-prof:'.length)
        const heapProf = args.find(a => a.startsWith('heap-prof:'))?.slice('heap-prof:'.length)
        const coverage = args.find(a => a.startsWith('coverage:'))?.slice('coverage:'.length)
        if (prof || heapProf || coverage) return report(await profiled(prof, heapProf, () => testEngine(args[1], args.slice(1)), coverage))
    }
    if (filter === '--engine') {
        if (args[1] === 'all') {
            let failures = 0
            const hasNativeOption = new Set(['cg', 'containers'])
            const first = crashResume ? Math.max(0, allTestableFiles.indexOf(crashResume.suite)) : 0
            for (const x of allTestableFiles.slice(first)) {
                const a = args.slice(2)
                if (hasNativeOption.has(x)) a.unshift('native')
                a.unshift(x)
                if (!crashResume || crashResume.suite !== x || crashResume.case === 0) console.log(`--- running ${a.join(' ')} ---`)
                failures = await testEngine(x, a) // let it fail fast
            }
            report(failures)
            if (!crashFd) process.on('beforeExit', () => console.log(`TOTAL TIME ${process.uptime().toFixed(3)}s`))
            return
        }
        return report(await testEngine(args[1], args.slice(1)))
    }

    const shouldExecute = args[1] === '--reify'

    for (const f of files) {
        if (filter) {
            if (!f.includes(filter)) continue
        }
        if (path.basename(f).startsWith('_')) continue

        console.log('running test case', f)
        if (f.endsWith('.vson')) {
            await runVsonTestCase(f)
            continue
        }
        await runTestCase(f, { shouldExecute })
    }
}

if (require.main === module) {
    launcher = [process.execPath, ...process.execArgv, process.argv[1]]
    main(...process.argv.slice(2)).catch(err => {
        console.error(err)
        process.exitCode = 1
    })
}
