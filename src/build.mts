import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import { execFile } from 'node:child_process'
import { promisify } from 'node:util'
import ts from 'typescript'

const run = promisify(execFile)

const tsDir = path.resolve('node_modules', 'typescript', 'lib')
const tsDeclarationFilePath = path.resolve(tsDir, 'typescript.d.ts')

async function getLibFiles() {
    const files = await fs.readdir(tsDir)

    return files.filter(f => f.startsWith('lib.') && f.endsWith('.d.ts')).map(f => path.resolve(tsDir, f))
}

async function copyLibFiles() {
    const files = await getLibFiles()
    const destDir = path.resolve('out', 'lib')
    await fs.mkdir(destDir, { recursive: true })
    await Promise.all(files.map(f => fs.copyFile(f, path.resolve(destDir, path.basename(f)))))
}

async function copySynLibFiles() {
    const destDir = path.resolve('out', 'lib')
    await fs.mkdir(destDir, { recursive: true })
    const files = await fs.readdir(path.resolve('src', 'lib'))
    await Promise.all(files.map(f => fs.copyFile(path.resolve('src', 'lib', f), path.resolve(destDir, `lib.${f}`))))
}

async function copyFiles() {
    await fs.mkdir('out', { recursive: true })

    await Promise.all([
        copyLibFiles(),
        copySynLibFiles(),
        fs.copyFile(tsDeclarationFilePath, path.resolve('out', path.basename(tsDeclarationFilePath)))
    ])
}

function compileTs() {
    const config = ts.getParsedCommandLineOfConfigFile('tsconfig.json', undefined, {
        ...ts.sys,
        onUnRecoverableConfigFileDiagnostic: d => {
            throw new Error(ts.flattenDiagnosticMessageText(d.messageText, '\n'))
        },
    })
    if (!config) throw new Error('Failed to parse tsconfig.json')
    const result = ts.createProgram(config.fileNames, config.options).emit()
    if (result.emitSkipped) throw new Error('TypeScript emit was skipped')
}

const zigArchs: Record<string, string> = { arm64: 'aarch64', x64: 'x86_64' }
const zigOses: Record<string, string> = { darwin: 'macos', linux: 'linux-gnu' }

async function newestMtime(dir: string, ext: string) {
    const files = (await fs.readdir(dir)).filter(f => f.endsWith(ext))
    const stats = await Promise.all(files.map(f => fs.stat(path.resolve(dir, f))))

    return Math.max(...stats.map(s => s.mtimeMs))
}

async function compileZig() {
    const outfile = path.resolve('out', 'node', 'api.node')
    await fs.mkdir(path.dirname(outfile), { recursive: true })
    await fs.writeFile(path.resolve('out', 'api.zig.js'), `process.dlopen(module, require('node:path').resolve(__dirname, 'node', 'api.node'))\n`)

    const built = await fs.stat(outfile).then(s => s.mtimeMs, () => 0)
    if (built > await newestMtime('src', '.zig')) return

    const arch = zigArchs[process.arch]
    const os = zigOses[process.platform]
    if (!arch || !os) throw new Error(`No Zig target for ${process.platform}-${process.arch}`)

    const args = ['build-lib', '-dynamic', '-fallow-shlib-undefined', '-lc', '-target', `${arch}-${os}`]
    if (process.env.OPT_MODE) args.push('-O', process.env.OPT_MODE)

    const localZig = path.resolve('node_modules', '.bin', 'zig')
    const zig = process.env.ZIG ?? (await fs.stat(localZig).then(() => localZig, () => 'zig'))
    await run(zig, [...args, 'src/api.zig', `-femit-bin=${outfile}`], { maxBuffer: 64 * 1024 * 1024 }).catch(e => {
        throw new Error(`Failed to compile src/api.zig with ${zig}\n${e.stderr ?? e.message}`)
    })
}

async function getSyntaxKinds() {
    const apiFile = ts.createSourceFile('api.ts', await fs.readFile(path.resolve('src', 'api.ts'), 'utf-8'), ts.ScriptTarget.ESNext)
    const decl = apiFile.statements.find(s => ts.isEnumDeclaration(s) && s.name.text === 'SyntaxKind') as ts.EnumDeclaration | undefined
    if (!decl) {
        throw new Error('Missing SyntaxKind in src/api.ts')
    }

    const kinds = new Map<string, number>()
    for (const member of decl.members) {
        const name = (member.name as ts.Identifier).text
        const init = member.initializer
        if (init && ts.isNumericLiteral(init)) kinds.set(name, Number(init.text))
        else if (init && ts.isIdentifier(init)) kinds.set(name, kinds.get(init.text)!)
        else throw new Error(`Unsupported SyntaxKind member: ${name}`)
    }

    return kinds
}

async function codeGen() {
    const kinds = await getSyntaxKinds()
    const tsDeclarationFile = ts.createSourceFile(
        tsDeclarationFilePath, 
        await fs.readFile(tsDeclarationFilePath, 'utf-8'),
        ts.ScriptTarget.ESNext
    )
    
    function getNamespaceStatements(name: string) {
        function getNamespace(name: string) {
            const n = tsDeclarationFile.statements.find(s => ts.isModuleDeclaration(s) && s.name.text == name)
            if (!n) {
                throw new Error(`Missing namespace: ${name}`)
            }
            return n as ts.ModuleDeclaration
        }    
    
        const n = getNamespace(name)
        if (!n.body || !ts.isModuleBlock(n.body)) {
            throw new Error(`Missing namespace body: ${name}`)
        }
    
        return n.body.statements
    }

    const statements = getNamespaceStatements('ts')
    
    function findInterfaceDecl(name: string) {
        return statements.find(x => ts.isInterfaceDeclaration(x) && x.name.text === name) as ts.InterfaceDeclaration | undefined
    }
    
    const fns = statements
        .filter(ts.isFunctionDeclaration)
        .filter(d => d.name && d.name.text.startsWith('is') && d.parameters.length === 1)
        .filter(d => d.type && ts.isTypePredicateNode(d.type) && ts.isIdentifier(d.type.parameterName) && d.type.parameterName.text === 'node')
        .map(d => [d, (d.type as ts.TypePredicateNode).type!] as const)

    const result: ts.Statement[] = []

    result.push(ts.factory.createImportDeclaration(
        undefined,
        ts.factory.createImportClause(true, ts.factory.createIdentifier('ts'), undefined),
        ts.factory.createStringLiteral('typescript')
    ))

    function getTsQualifiedNameReference(name: string) {
        return ts.factory.createTypeReferenceNode(
            ts.factory.createQualifiedName(ts.factory.createIdentifier('ts'), name)
        )
    }

    const y = fns.map(x => {
        if (ts.isTypeReferenceNode(x[1])) {
            const name = x[1].typeName
            if (ts.isIdentifier(name)) {
                const decl = findInterfaceDecl(name.text)
                if (!decl) return

                const kind = decl.members.find(x => x.name && ts.isIdentifier(x.name) && x.name.text == 'kind' && ts.isPropertySignature(x)) as ts.PropertySignature | undefined
                const type = kind?.type
                if (!type || !ts.isTypeReferenceNode(type)) return
                
                const inner = type.typeName
                if (ts.isQualifiedName(inner)) {
                    if (inner.right.text.startsWith('JSDoc')) return
                    const val = kinds.get(inner.right.text)
                    if (val === undefined) return

                    result.push(ts.factory.createFunctionDeclaration(
                        [ts.factory.createModifier(ts.SyntaxKind.ExportKeyword)],
                        undefined,
                        x[0].name!,
                        undefined,
                        [ts.factory.createParameterDeclaration(
                            undefined,
                            undefined,
                            'node',
                            undefined,
                            getTsQualifiedNameReference('Node'),
                            undefined,
                        )],
                        ts.factory.createTypePredicateNode(undefined, 'node', getTsQualifiedNameReference(name.text)),
                        ts.factory.createBlock([
                            ts.factory.createReturnStatement(
                                ts.factory.createBinaryExpression(
                                    ts.factory.createPropertyAccessExpression(
                                        ts.factory.createIdentifier('node'),
                                        'kind'
                                    ),
                                    ts.SyntaxKind.EqualsEqualsEqualsToken,
                                    ts.factory.createNumericLiteral(val)
                                )
                            )
                        ], true)
                    ))
                } 
            }
        }
    })

    const createSourcefile = () => ts.factory.createSourceFile([], ts.factory.createToken(ts.SyntaxKind.EndOfFileToken), ts.NodeFlags.None)

    function printNodes(nodes: readonly ts.Node[], source = nodes[0].getSourceFile() ?? createSourcefile(), options?: ts.PrinterOptions) {
        const printer = ts.createPrinter(options)
        const result = nodes.map(n => printer.printNode(ts.EmitHint.Unspecified, n, source))
    
        return result.join('\n')
    }

    await fs.writeFile('src/predicates.ts', printNodes(result))
}

async function main() {
    await Promise.all([
        copyFiles(),
        codeGen().then(compileTs),
        compileZig(),
    ])
}

await main()

