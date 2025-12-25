import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import ts from 'typescript'

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

async function copyFiles() {
    await fs.mkdir('out', { recursive: true })

    await Promise.all([
        copyLibFiles(),
        fs.copyFile(tsDeclarationFilePath, path.resolve('out', path.basename(tsDeclarationFilePath)))
    ])
}

// FIXME: duplicate `SyntaxKind` into predicates

async function codeGen() {
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
                    const val = ts.SyntaxKind[inner.right.text as any]

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

export async function main() {
    await Promise.all([
        copyFiles(),
        codeGen(),
    ])
}
