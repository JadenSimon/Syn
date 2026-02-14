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

// export function f5<T extends string, U = T>(a: U) {
//     type Q<K> = K | T
//     type X = { x: Q<T> }
//     const x: X = { x: a }
//     return x
// }

export function f5() {
    let x: { z: number } | { y: { z: number } } | undefined
    const f1 = <T>(y: T) => ({ y })
    const f2 = () => ({ z: 1 })
    const q = false ? x ??= f1(f2()) : f2()
    return q
}

// T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9;
export function f6<T1, T2, T3, T4, T5, T6, T7, T8, T9>(a: T9) {
    return a
}

export const x6 = f6(1)
// export type Y = X<1,2,3,4,5,6,7,8,9>

export function f10() {
    let x: 1 | 2 | 3 = 1 
    let y: 1 | 2 | 3 = 1
    while (x !== 3) {
        x = y * 4
    }
    return x
}
