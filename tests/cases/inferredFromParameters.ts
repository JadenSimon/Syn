// @filename: foo.ts
export function foo() {
    return _foo1('hi')
}

export function foo2() {
    return _foo2('hi')
}

export function foo3() {
    return _foo1<string>('hi')
}

export function foo4() {
    return _foo2<string>('hi')
}

export function foo5() {
    return _foo2<'hi'>('hi')
}

export function foo6() {
    return _foo1('hi' as const)
}

function _foo1<T>(a: T) {
    return a
}

function _foo2<T>(a: T): T {
    return a
}

export function _foo3<T>(a: T) {
    return a
}

function _mapped<T extends Record<string, any>>(a: T): { [P in keyof T]: T[P] } {
    return a
}

export function mapped1() {
    return _mapped({ hi: 'there' })
}

export function _mapped2<T extends Record<string, any>>(a: T): { [P in keyof T]: { x: T[P] } } {
    return a
}

export function mapped2() {
    return _mapped2({ hi: 'there' })
}

// needed unless we add lib reference
type Record<K extends keyof any, T> = {
    [P in K]: T;
};

export function shaped<T extends boolean | number>(s: { t: T }) {
    return s.t
}

// todo: tsc evals these in const context
export const shaped_1 = shaped({ t: true })
export const shaped_2 = shaped({ t: 1 })

// export function shaped2<T extends { t: boolean }>(s: T) {
//     return s.t
// }

// export const shaped2_1 = shaped2({ t: true })
 
// type Q = { t: boolean; n: number }
// export function shaped2<T extends Q, K extends keyof T>(s: T, k: K) {
//     return s[k]
// }

// export const shaped2_1 = shaped2({ t: true, n: 1 as const }, 'n')
 
declare function wrap<T, U>(cb: (x: T) => U): (x: T) => U

function f2(a: (x: string) => number, b: string) {
    return a(b)
}

export const f3 = f2(wrap(s => s.length), 'a')

// FIXME: currently fails
// function f4<T, U>(a: (x: T) => U, b: T) {
//     return a(b)
// }

// export const f5 = f4(wrap(s => s.length), 'a')