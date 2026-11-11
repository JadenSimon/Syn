import { spawn } from 'node:child_process'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'

const usage = 'usage: node src/measure.mts <out> [rounds:N] [compiles:N] [jobs:N] <label>=<tree>[,<flag>...] ...'
const suites = ['mini-optimizer', 'maps', 'floating', 'lowering-attempts']
const engine = [process.execPath, '--experimental-vm-modules', '--disable-warning=ExperimentalWarning', 'out/testRunner.js', '--engine']
const control = 'aa'

interface Version {
    label: string
    tree: string
    flags: string[]
    images: string
}

interface Task {
    name: string
    args: string[]
    env?: Record<string, string>
}

interface Ran {
    ok: boolean
    text: string
}

function option(args: string[], name: string, fallback: number): number {
    const found = args.find(a => a.startsWith(`${name}:`))
    return found ? Number(found.slice(name.length + 1)) : fallback
}

function run(tree: string, command: string[], env: Record<string, string>, log: string, seconds: number): Promise<Ran> {
    return new Promise(resolve => {
        const child = spawn(command[0], command.slice(1), { cwd: tree, env: { ...process.env, ...env }, timeout: seconds * 1000, killSignal: 'SIGKILL' })
        const chunks: Buffer[] = []
        child.stdout.on('data', chunk => chunks.push(chunk))
        child.stderr.on('data', chunk => chunks.push(chunk))
        child.on('close', status => {
            const text = Buffer.concat(chunks).toString('latin1')
            fs.writeFileSync(log, text)
            resolve({ ok: status === 0, text })
        })
    })
}

function buildTasks(version: Version): Task[] {
    const image = [`image-out:${version.images}`, 'native-only', 'samples:1', 'warmups:0', 'timeout:300']
    const tasks: Task[] = [
        { name: 'counts', args: ['bootstrap-test', 'only:l2Normalize', 'timeout:300', 'arch-census', 'flow-cost'] },
        { name: 'boot', args: ['bootstrap-test', 'bench:l2Normalize', 'only:l2Normalize', ...image] },
    ]
    for (const suite of suites) tasks.push({ name: suite, args: [suite, 'bench', ...image] })
    tasks.push({ name: 'normalize', args: ['heap', 'timeout:300', 'skip:-'], env: { SYN_L2_BUILD: `${version.images}/normalize.img` } })
    return tasks
}

async function build(out: string, versions: Version[], jobs: number): Promise<void> {
    const pending: [Version, Task][] = []
    for (const version of versions) {
        fs.mkdirSync(version.images, { recursive: true })
        for (const task of buildTasks(version)) {
            if (!fs.existsSync(`${version.images}/${task.name}.done`)) pending.push([version, task])
        }
    }
    async function worker(): Promise<void> {
        while (pending.length) {
            const [version, task] = pending.shift()!
            const log = `${out}/log/${version.label}-${task.name}.txt`
            const ran = await run(version.tree, [...engine, ...task.args, ...version.flags], task.env ?? {}, log, 900)
            const made = ran.ok && (task.name !== 'normalize' || fs.existsSync(`${version.images}/normalize.img`))
            if (made) fs.writeFileSync(`${version.images}/${task.name}.done`, '')
            console.log(`built ${version.label} ${task.name}: ${made ? 'ok' : `FAILED, see ${log}`}`)
        }
    }
    const workers: Promise<void>[] = []
    for (let i = 0; i < jobs; i++) workers.push(worker())
    await Promise.all(workers)
}

function built(version: Version, task: string): boolean {
    return fs.existsSync(`${version.images}/${task}.done`)
}

function load(): string {
    return os.loadavg()[0].toFixed(2)
}

function lastRound(file: string): number {
    if (!fs.existsSync(file)) return 0
    let last = 0
    for (const line of fs.readFileSync(file, 'utf8').split('\n')) {
        const round = Number(line.split('\t')[0])
        if (round > last) last = round
    }
    return last
}

function rotated<T>(list: T[], by: number): T[] {
    const out: T[] = []
    for (let k = 0; k < list.length; k++) out.push(list[(k + by) % list.length])
    return out
}

async function compileRounds(out: string, versions: Version[], rounds: number): Promise<void> {
    const file = `${out}/compile.tsv`
    const first = lastRound(file) + 1
    const labels = [...versions.map(v => v.label), control]
    for (let round = first; round < first + rounds; round++) {
        for (const label of rotated(labels, round)) {
            const version = label === control ? versions[0] : versions.find(v => v.label === label)!
            const args = ['bootstrap-test', 'only:l2Normalize', 'timeout:300', 'trace-stats', ...version.flags]
            const ran = await run(version.tree, ['/usr/bin/time', '-p', ...engine, ...args], {}, `${out}/log/${label}-compile.txt`, 900)
            const phase = (name: string) => Number(new RegExp(`^  ${name}: ([0-9.]+)`, 'm').exec(ran.text)?.[1])
            const user = Number(/^user\s+([0-9.]+)/m.exec(ran.text)?.[1]) * 1000
            if (!ran.ok || !/^RESULT ok/m.test(ran.text)) {
                console.log(`compile ${round} ${label}: FAILED`)
                continue
            }
            for (const name of ['analysis', 'plan', 'total']) fs.appendFileSync(file, `${round}\t${label}\tcompile\t${name}\t${phase(name).toFixed(1)}\t${load()}\n`)
            fs.appendFileSync(file, `${round}\t${label}\tcompile\tuser\t${user.toFixed(0)}\t${load()}\n`)
            console.log(`compile ${round} ${label}: total ${(phase('total') / 1000).toFixed(1)}s`)
        }
    }
}

async function capture(out: string, version: Version): Promise<string> {
    const dir = `${out}/graphs`
    if (fs.existsSync(`${dir}.done`)) return dir
    fs.rmSync(dir, { recursive: true, force: true })
    const ran = await run(version.tree, [...engine, 'lowering-attempts', `l2-capture:${dir}`, 'skip:-', 'timeout:300'], {}, `${out}/log/capture.txt`, 900)
    if (ran.ok) fs.writeFileSync(`${dir}.done`, '')
    console.log(`captured normalize inputs of lowering-attempts on ${version.label}: ${ran.ok ? `${fs.readdirSync(dir).length} graphs` : 'FAILED'}`)
    return dir
}

async function runtimeRounds(out: string, versions: Version[], rounds: number, graphs: string): Promise<void> {
    const file = `${out}/runtime.tsv`
    const first = lastRound(file) + 1
    const labels = [...versions.map(v => v.label), control]
    for (let round = first; round < first + rounds; round++) {
        for (const label of rotated(labels, round)) {
            const version = label === control ? versions[0] : versions.find(v => v.label === label)!
            const image = [`image-in:${version.images}`, 'native-only', 'timeout:300']
            const benches: [suite: string, task: string, args: string[]][] = [['boot', 'boot', ['bootstrap-test', 'bench:l2Normalize', 'only:l2Normalize', 'samples:100', ...image]]]
            for (const suite of suites) benches.push([suite, suite, [suite, 'bench', 'samples:30', ...image]])
            for (const [suite, task, args] of benches) {
                if (!built(version, task)) continue
                const ran = await run(version.tree, [...engine, ...args], {}, `${out}/log/${label}-run-${suite}.txt`, 600)
                for (const line of ran.text.split('\n')) {
                    const bench = /^bench (.+?) compile \d+ms, native ([0-9.]+) us/.exec(line)
                    if (bench) fs.appendFileSync(file, `${round}\t${label}\t${suite}\t${bench[1].replaceAll(', ', ',')}\t${bench[2]}\t${load()}\n`)
                }
            }
            if (built(version, 'normalize') && fs.existsSync(`${graphs}.done`)) {
                const ran = await run(version.tree, [...engine, 'heap', `l2-replay:${graphs}`, 'l2-rounds:3', 'skip:-', 'timeout:300'], { SYN_L2_WITH: `${version.images}/normalize.img` }, `${out}/log/${label}-run-replay.txt`, 600)
                let native = 0
                let count = 0
                for (const line of ran.text.split('\n')) {
                    if (!line.startsWith('L2B ')) continue
                    native += Number(line.split(' ')[8])
                    count++
                }
                const identical = /^L2REPLAY (\d+)\/(\d+) identical/m.exec(ran.text)
                if (identical && count) fs.appendFileSync(file, `${round}\t${label}\treplay\t${count} graphs${identical[1] === identical[2] ? '' : `, ${Number(identical[2]) - Number(identical[1])} differ from host`}\t${native.toFixed(2)}\t${load()}\n`)
            }
            console.log(`runtime ${round} ${label}`)
        }
    }
}

function median(values: number[]): number {
    const sorted = [...values].sort((a, b) => a - b)
    const mid = sorted.length >> 1
    return sorted.length % 2 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2
}

function deviation(values: number[]): number {
    if (values.length < 2) return Infinity
    let mean = 0
    for (const value of values) mean += value / values.length
    let squares = 0
    for (const value of values) squares += (value - mean) ** 2
    return Math.sqrt(squares / (values.length - 1))
}

const quantiles = [12.71, 4.3, 3.18, 2.78, 2.57, 2.45, 2.36, 2.31, 2.26, 2.23, 2.2, 2.18, 2.16, 2.14, 2.13, 2.12, 2.11, 2.1, 2.09, 2.09, 2.08, 2.07, 2.07, 2.06, 2.06, 2.06, 2.05, 2.05, 2.05, 2.04]

function medianError(spread: number, rounds: number): number {
    if (rounds < 2) return Infinity
    return (quantiles[rounds - 2] ?? 2) * 1.2533 * spread / Math.sqrt(rounds)
}

function roundsFor(spread: number, target: number): number {
    let rounds = 2
    while (rounds < 1000 && medianError(spread, rounds) > target) rounds++
    return rounds
}

type Rounds = Map<string, Map<number, number>>

function readRows(file: string): Map<string, Rounds> {
    const rows = new Map<string, Rounds>()
    if (!fs.existsSync(file)) return rows
    for (const line of fs.readFileSync(file, 'utf8').split('\n')) {
        const fields = line.split('\t')
        if (fields.length < 6) continue
        const key = `${fields[2]}\t${fields[3]}`
        let row = rows.get(key)
        if (!row) rows.set(key, row = new Map())
        let byRound = row.get(fields[1])
        if (!byRound) row.set(fields[1], byRound = new Map())
        byRound.set(Number(fields[0]), Number(fields[4]))
    }
    return rows
}

function ratios(row: Rounds, label: string, reference: string): Map<number, number> {
    const out = new Map<number, number>()
    const base = row.get(reference)
    for (const [round, value] of row.get(label) ?? []) {
        const against = base?.get(round)
        if (against) out.set(round, value / against)
    }
    return out
}

function geomeanRow(rows: Rounds[], labels: string[], reference: string): Rounds {
    const out: Rounds = new Map()
    for (const label of labels) {
        const logs = new Map<number, number[]>()
        for (const row of rows) {
            for (const [round, ratio] of ratios(row, label, reference)) {
                let list = logs.get(round)
                if (!list) logs.set(round, list = [])
                list.push(Math.log(ratio))
            }
        }
        const byRound = new Map<number, number>()
        for (const [round, list] of logs) {
            let sum = 0
            for (const value of list) sum += value
            byRound.set(round, Math.exp(sum / list.length))
        }
        out.set(label, byRound)
    }
    const ones = new Map<number, number>()
    for (const round of out.get(control)?.keys() ?? []) ones.set(round, 1)
    out.set(reference, ones)
    return out
}

function roundCount(rows: Map<string, Rounds>): number {
    let most = 0
    for (const row of rows.values()) {
        for (const byRound of row.values()) most = Math.max(most, byRound.size)
    }
    return most
}

function resolved(row: Rounds, reference: string): boolean {
    const base = [...(row.get(reference)?.values() ?? [])]
    return base.length > 0 && median(base) >= 1
}

function percent(ratio: number): string {
    const value = (ratio - 1) * 100
    return `${value >= 0 ? '+' : ''}${value.toFixed(2)}%`
}

function reportRow(name: string, unit: string, row: Rounds, labels: string[], reference: string): { line: string, far: boolean } {
    const noise = [...ratios(row, control, reference).values()]
    const band = noise.length ? Math.max(Math.abs(median(noise) - 1), medianError(deviation(noise), noise.length)) : Infinity
    const needed = noise.length < 2 ? Infinity : roundsFor(deviation(noise), 0.01)
    const base = [...(row.get(reference)?.values() ?? [])]
    let line = `${name.padEnd(44)}${(base.length && unit ? `${median(base).toFixed(2)} ${unit}` : '').padStart(14)}`
    let far = false
    for (const label of labels) {
        const list = [...ratios(row, label, reference).values()]
        if (!list.length) {
            line += 'none'.padStart(18)
            continue
        }
        const mid = median(list)
        const outside = label !== control && Math.abs(mid - 1) > band
        if (label !== control && Math.abs(mid - 1) > 2 * band) far = true
        line += `${percent(mid)} ${label === control || !Number.isFinite(band) ? '      ' : !outside ? 'same  ' : mid > 1 ? 'slower' : 'faster'}`.padStart(18)
    }
    line += `${Number.isFinite(band) ? `±${(band * 100).toFixed(2)}%` : '-'}`.padStart(10)
    line += `${Number.isFinite(needed) ? String(needed) : '-'}`.padStart(8)
    return { line, far }
}

function counts(out: string, version: Version): string {
    const file = `${out}/log/${version.label}-counts.txt`
    const text = fs.existsSync(file) ? fs.readFileSync(file, 'latin1') : ''
    const unit = /^native unit (\d+) fns (\d+)\/(\d+) instructions.*$/m.exec(text)
    const work = /^flow cost: work=(\d+) .* instances=(\d+)/m.exec(text)
    const inlined = /\((\d+) inlined away\)/.exec(unit?.[0] ?? '')
    const units = /^ARCH units (\d+) over/m.exec(text)
    const census = text.split('\n').filter(line => line.startsWith('ARCH '))
    if (census.length) fs.writeFileSync(`${out}/census-${version.label}.txt`, `${census.join('\n')}\n`)
    if (!unit) return `${version.label.padEnd(12)}no boot compile, see ${file}`
    return `${version.label.padEnd(12)}${`${unit[2]}/${unit[3]}`.padStart(16)}${unit[1].padStart(8)}${(inlined?.[1] ?? '-').padStart(10)}${(units?.[1] ?? '-').padStart(8)}${(work?.[1] ?? '-').padStart(12)}${(work?.[2] ?? '-').padStart(13)}`
}

function report(out: string, versions: Version[]): void {
    const reference = versions[0].label
    const labels = [control, ...versions.slice(1).map(v => v.label)]
    const header = (title: string) => `${title.padEnd(44)}${reference.padStart(14)}${labels.map(label => label.padStart(18)).join('')}${'band'.padStart(10)}${'1% at'.padStart(8)}`
    const lines: string[] = []
    const detail: string[] = []
    lines.push('boot compile, exact counts')
    lines.push(`${''.padEnd(12)}${'insts/non-mov'.padStart(16)}${'fns'.padStart(8)}${'inlined'.padStart(10)}${'units'.padStart(8)}${'flow work'.padStart(12)}${'flow copies'.padStart(13)}`)
    for (const version of versions) lines.push(counts(out, version))
    for (const version of versions) {
        for (const task of buildTasks(version)) {
            if (!built(version, task.name)) lines.push(`${version.label} ${task.name}: not built, see ${out}/log/${version.label}-${task.name}.txt`)
        }
    }
    const compile = readRows(`${out}/compile.tsv`)
    if (compile.size) {
        lines.push('', header('boot compile time, ms'))
        for (const [key, row] of compile) lines.push(reportRow(key.split('\t')[1], 'ms', row, labels, reference).line)
    }
    const runtime = readRows(`${out}/runtime.tsv`)
    if (runtime.size) {
        lines.push('', header('runtime'))
        detail.push(header('runtime, every bench'))
        const bySuite = new Map<string, [name: string, row: Rounds][]>()
        for (const [key, row] of runtime) {
            const [suite, name] = key.split('\t')
            let list = bySuite.get(suite)
            if (!list) bySuite.set(suite, list = [])
            list.push([name, row])
        }
        const outside: string[] = []
        for (const [suite, list] of bySuite) {
            for (const [name, row] of list) {
                const shown = reportRow(`${suite} ${name}`, suite === 'replay' ? 'ms' : 'us', row, labels, reference)
                detail.push(shown.line)
                if (list.length === 1) lines.push(shown.line)
                else if (shown.far && resolved(row, reference)) outside.push(shown.line)
            }
            const kept = list.filter(([, row]) => resolved(row, reference)).map(([, row]) => row)
            if (list.length > 1) lines.push(reportRow(`${suite}, geomean of ${kept.length}`, '', geomeanRow(kept, labels, reference), labels, reference).line)
        }
        if (outside.length) lines.push('', 'single benches outside twice their band', ...outside)
    }
    lines.push('', `${roundCount(compile)} compile rounds, ${roundCount(runtime)} runtime rounds; each cell is the median over rounds of that round's ratio to ${reference}; ${control} is ${reference} run again in the same rotation`)
    lines.push(`band = the larger of ${control}'s offset from 0 and the 95% error of its median ratio (t quantile x 1.2533 x spread / sqrt(rounds)); inside the band reads same; 1% at = rounds for that error to reach 1%`)
    lines.push('a bench under 1 us a call prints fewer than three digits and is left out of its geomean; single benches are listed at twice their band because a few of many leave it by chance; every bench is in the second table of report.txt')
    fs.writeFileSync(`${out}/report.txt`, `${lines.join('\n')}\n\n${detail.join('\n')}\n`)
    console.log(lines.join('\n'))
}

async function main(): Promise<void> {
    const args = process.argv.slice(2)
    const given = args.filter(a => a.includes('=')).map(a => {
        const at = a.indexOf('=')
        const [tree, ...flags] = a.slice(at + 1).split(',')
        return { label: a.slice(0, at), tree: path.resolve(tree), flags }
    })
    if (!args.length || args[0].includes('=') || args[0].includes(':') || !given.length || given.some(v => v.label === control)) {
        console.log(usage)
        process.exitCode = 1
        return
    }
    const out = path.resolve(args[0])
    const versions: Version[] = given.map(v => ({ ...v, images: `${out}/images/${v.label}` }))
    fs.mkdirSync(`${out}/log`, { recursive: true })
    const rounds = option(args, 'rounds', 12)
    const compiles = option(args, 'compiles', 3)
    if (rounds || compiles) await build(out, versions, option(args, 'jobs', 2))
    const graphs = rounds ? await capture(out, versions[0]) : `${out}/graphs`
    await compileRounds(out, versions, compiles)
    await runtimeRounds(out, versions, rounds, graphs)
    report(out, versions)
}

main()
