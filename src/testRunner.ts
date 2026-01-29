import * as api from './api'
import * as fs from 'node:fs'
import * as path from 'node:path'
import * as crypto from 'node:crypto'
import { runSynModule } from './reifiedTypes'

const filenameMarker = '// @filename:'

interface VirtualFile {
    name: string
    text: string
}

function parseTestFile(text: string, extname = '.ts') {
    const compilerOptions: any = {
        declaration: true,
    }

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
    }

    let currentFile: any = {}
    const files: VirtualFile[] = []

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
                files.push({
                    ...currentFile,
                    text: t,
                })
            }
            currentFile = {}
        }

        const eol = text.indexOf('\n', nextMarker)
        currentFile.name = text.slice(nextMarker + filenameMarker.length, eol).trim()
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
    const proposedDir = path.resolve(testsDir, 'proposed')

    const absPath = path.resolve(casesDir, name)
    const relPath = path.relative(casesDir, absPath)

    const text = await fs.promises.readFile(absPath, 'utf-8')
    const parsed = parseTestFile(text, path.extname(absPath))
    const vfs = getVfs()
    vfs.files.clear()

    const testHash = crypto.hash('sha256', text, 'hex')

    const roots: string[] = []
    for (const item of parsed.files) {
        const resolved = path.resolve(testsDir, item.name)
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
        group.push({ name, text: f.text, order: roots.indexOf(f.source), absPath: f.source })
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

        if (opt?.shouldExecute && k === '.js') {
            const f = v[0]
            const reifier = (prog as any).getReifier()
            runSynModule(f.text, f.absPath, reifier)
        }

        if (existingSnapshot === undefined) {
            await writeFile(snapshotPath, newSnapshot)
            continue
        }

        const diff = lineDiff(existingSnapshot, newSnapshot)
        if (diff.length === 0) {
            // all good
            continue
        }

        console.log(diff)

        if (!opt?.testOnly) {
            //const proposedPath = path.resolve(proposedDir, relPath.replace(/\.tsx?$/, k))
            await writeFile(snapshotPath, newSnapshot)
        }

        // if (!opt?.noWrite) {
        //     await fs.promises.writeFile(snapshotPath, newSnapshot)
        // }
    }
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

export async function main(...args: string[]) {
    const files = await gatherTestCases()
    const filter = args[0]
    const shouldExecute = args[1] === '--reify'

    for (const f of files) {
        if (filter) {
            if (!f.includes(filter)) continue
        }
        if (path.basename(f).startsWith('_')) continue

        console.log('running test case', f)
        await runTestCase(f, { shouldExecute })
    }
}