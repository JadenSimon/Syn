import * as vm from 'node:vm'
import * as v8 from 'node:v8'
import * as path from 'node:path'
import { SourceMap } from 'node:module'

export interface Reifier {
    types: any
    __reify: any
    __readFile?: (file: string) => string
    __writeFile?: (file: string, text: string) => void
    __readDir?: (dir: string) => string[]
}

export type ResolveModule = (from: string, name: string) => [absPath: string, text: string | undefined] | undefined

function createSynContext(reifier: Reifier, extra: Record<string, unknown>) {
    ;(globalThis as any).Type = reifier.types
    return vm.createContext({
        Type: reifier.types,
        __reify: reifier.__reify,
        __readFile: reifier.__readFile,
        __writeFile: reifier.__writeFile,
        __readDir: reifier.__readDir,
        console: console,
        performance,
        process,
        setTimeout,
        Buffer,
        v8,
        JSON,
        TextDecoder,
        TextEncoder,
        exports: {},
        ...extra,
    })
}

export function runSynScript(text: string, fileName: string, reifier: Reifier) {
    const ctx = createSynContext(reifier, { __filename: fileName })
    const source = '"use strict";\n' + text
    try {
        return vm.runInContext(source, ctx, { filename: fileName, lineOffset: -1 })
    } catch (err) {
        applySourceMaps(new Map([[fileName, source]]), err as Error)
        throw err
    }
}

export function createModuleLoader(reifier: Reifier, resolve: ResolveModule, ts: any) {
    const ctx = createSynContext(reifier, { ts, __argv: [] })
    const modules = new Map<string, vm.Module>()
    const sources = new Map<string, string>()
    const finished = new Set<vm.Module>()
    const pending = new Map<vm.Module, Promise<unknown>>()
    const linking = new Map<vm.Module, Promise<void>>()
    const readyMarker = 'await import.meta.ready();'
    const finishedMarker = '\nimport.meta.finished()\n'

    function dependsOn(m: vm.Module | undefined, matches: (m: vm.Module) => boolean, seen = new Set<vm.Module>()): boolean {
        if (!m || seen.has(m)) return false
        seen.add(m)
        if (matches(m)) return true
        for (const spec of (m as vm.SourceTextModule).dependencySpecifiers ?? []) {
            const p = resolve(m.identifier, spec)
            if (!p) continue
            if (dependsOn(modules.get(p[0]), matches, seen)) return true
        }
        return false
    }

    function isEvaluating(m: vm.Module) {
        if (m.status === 'evaluating') return true
        return m instanceof vm.SourceTextModule && m.status === 'evaluated' && !finished.has(m)
    }

    async function evaluated(m: vm.Module | undefined) {
        if (!m) return m
        if (m.status === 'unlinked' && !linking.has(m)) linking.set(m, m.link(link))
        await linking.get(m)
        if (m.status !== 'linked') return m
        const cyclic = dependsOn(m, isEvaluating)
        const done = m.evaluate()
        if (!cyclic) {
            await done
            return m
        }
        pending.set(m, done)
        done.then(() => pending.delete(m), () => pending.delete(m))
        return m
    }

    function instantiate(absPath: string, text: string): vm.SourceTextModule {
        const source = `let __filename = import.meta.filename; ${readyMarker}\n` + text + finishedMarker
        sources.set(absPath, source)
        return new vm.SourceTextModule(source, {
            identifier: absPath,
            context: ctx,
            importModuleDynamically: (async (spec: string, from: vm.Module) => evaluated(await link(spec, from))) as any,
            initializeImportMeta: (meta: any, m) => {
                meta.filename = m.identifier.replace(/\.js$/, '.syn')
                meta.dirname = path.dirname(meta.filename)
                meta.finished = () => finished.add(m)
                meta.ready = () => {
                    const waits = [...pending].filter(([p]) => dependsOn(m, x => x === p) && !dependsOn(p, x => x === m)).map(([, done]) => done)
                    if (waits.length) return Promise.all(waits)
                }
            },
            lineOffset: -1,
        })
    }

    function synthesizeTypescript(): vm.Module {
        const exports = Object.keys(ts)
        if (!exports.includes('default')) exports.push('default')
        return new vm.SyntheticModule(exports, function() {
            for (const k of exports) this.setExport(k, ts[k])
            this.setExport('default', ts)
        }, { context: ctx })
    }

    async function link(spec: string, from: vm.Module): Promise<vm.Module> {
        if (spec === 'typescript') {
            let cached = modules.get(spec)
            if (!cached) modules.set(spec, cached = synthesizeTypescript())
            return cached
        }
        const p = resolve(from.identifier, spec)
        if (!p || p[1] === undefined) throw new Error(`cannot resolve '${spec}' from ${from.identifier}`)
        const cached = modules.get(p[0])
        if (cached) return cached
        const m = instantiate(p[0], p[1])
        modules.set(p[0], m)
        return m
    }

    async function run(absPath: string, text: string, argv: string[]): Promise<unknown> {
        ctx.__argv = argv
        const reload = modules.has(absPath)
        const m = instantiate(absPath, text)
        if (!reload) modules.set(absPath, m)
        await m.link(link)
        return m.evaluate().catch(err => {
            applySourceMaps(sources, err)
            throw err
        })
    }

    return { run }
}

export function applySourceMaps(nameToSource: Map<string, string>, err: Error) {
    const trace = err.stack
    if (!trace) return

    const sourcemaps = new Map<string, any>()
    function getSourceMap(name: string) {
        const m = sourcemaps.get(name)
        if (m !== undefined) return m
        const src = nameToSource.get(name)
        if (!src) return

        const match = src.match(
            /\/\/[#@]\s*sourceMappingURL=data:application\/json(?:;charset=utf-8)?;base64,([^\s]+)/
        )
        const sourceMap = match
            ? new SourceMap(JSON.parse(Buffer.from(match[1], 'base64').toString('utf8')))
            : null
        sourcemaps.set(name, sourceMap)
        return sourceMap
    }

    err.stack = trace.replace(
        /([^\s\(:]+):(\d+):(\d+)/g,
        (_, name, line, column) => {
            const sourceMap = getSourceMap(name)
            if (!sourceMap) return _

            const mapped = sourceMap.findEntry(Number(line) - 1, Number(column) - 1)

            return `${mapped.originalSource ?? name}:${mapped.originalLine + 1}:${mapped.originalColumn + 1}`
        }
    )
}
