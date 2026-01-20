const isPrototypeProperty = true
const namespaceSymbol = {} as { members?: { symbols: {} }; exports?: { symbols: {} } }
const createSymbolTable = () => ({ symbols: {} })
export const symbolTable = isPrototypeProperty ?
    (namespaceSymbol.members || (namespaceSymbol.members = createSymbolTable())) :
    (namespaceSymbol.exports || (namespaceSymbol.exports = createSymbolTable()));


let x: { z: number } | { y: { z: number } } | undefined
const f1 = <T>(y: T) => ({ y })
const f2 = () => ({ z: 1 })
export const q = false ? x ??= f1(f2()) : f2()

export function f3() {
    const x = 1 as number | undefined | null
    const y = false as string | false
    const z = x && y // string | false | 0 | null | undefined
    return z
}

export function f4() {
    const x = undefined as { a: number } | undefined
    return x && x.a
}